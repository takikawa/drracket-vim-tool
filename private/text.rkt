#lang racket/gui

(require framework)

(define mode/c
  (one-of/c 'command 'insert 'visual))

(define on-local-char/c
  (->m (is-a?/c key-event%) void?))

(define on-paint/c
  (->m any/c (is-a?/c dc<%>) real? real? real? real? real? real?
       (or/c (one-of/c 'no-cart 'show-inactive-caret 'show-caret)
             (cons/c natural-number/c natural-number/c))
       void?))

(provide/contract
  [vim-emulation<%> interface?]
  [vim-emulation-mixin
    (-> (class/c
          (inherit invalidate-bitmap-cache
                   get-position 
                   set-position 
                   move-position 
                   copy paste kill undo redo delete
                   line-start-position line-end-position position-line
                   get-view-size local-to-global
                   find-wordbreak get-admin
                   get-style-list get-padding)
          (super on-local-char on-paint))
      (class/c
        [on-paint on-paint/c]
        [on-local-char on-local-char/c]
        (override [on-paint on-paint/c]
                  [on-local-char on-local-char/c])
        [vim? (->m boolean?)]
        [toggle-vim! (->m void?)]
        [set-mode! (->m mode/c void?)]
        [get-mode  (->m mode/c)]))])

(define vim-emulation<%>
  (interface ()
    vim? 
    toggle-vim!
    set-mode!
    get-mode))

(define vim-emulation-mixin
  (mixin (text:basic<%>) (vim-emulation<%>)

    ;; ==== public state & accessors ====
    (inherit invalidate-bitmap-cache)

    ;; vim-style mode 
    ;; (one-of/c 'command 'insert 'visual)
    (define mode 'command)

    (define/public-final (set-mode! new-mode)
      (set! mode new-mode)
      (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end))

    (define/public-final (get-mode)
      mode)

    (define/public-final (vim?) vim-emulation?)

    (define/public-final (toggle-vim!)
      (preferences:set 'drracket:vim-emulation? (not vim-emulation?))
      (set! vim-emulation? (not vim-emulation?)))

    ;; ==== private state ====
    (define vim-emulation? (preferences:get 'drracket:vim-emulation?))

    (define mode-padding 3)
    (define old-clipping #f)

    ;; save the dc state so we don't mangle it for others
    (define-struct dc-state (pen font fg))
    (define saved-dc-state #f)
    
    ;; continuation into queued command handler
    (define command-continuation #f)

    ;; ==== overrides ====
    ;; override character handling and dispatch based on mode
    ;; is-a?/c key-event% -> void?
    (define/override (on-local-char event)
      (if vim-emulation?
        (cond [(eq? mode 'command) (do-command event)]
              [(eq? mode 'insert)  (do-insert event)]
              [(eq? mode 'visual)  (do-visual event)]
              [else (error "Unimplemented")])
        (super on-local-char event)))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if vim-emulation?
        (begin
          (save-dc-state dc)
          (setup-dc dc)
          (if before?
            (fix-clipping dc left top right bottom dx dy)
            (begin
              ;; don't clip out mode line for *our* drawing
              (send dc set-clipping-region old-clipping)
              (draw-mode-line dc left top right bottom dx dy)))
          (restore-dc-state dc)
          (super on-paint before? dc left top right bottom dx dy draw-caret))
        (super on-paint before? dc left top right bottom dx dy draw-caret)))

    ;; ==== private functionality ====
    (inherit get-position set-position 
             move-position 
             copy paste kill undo redo delete
             line-start-position line-end-position position-line
             get-view-size local-to-global
             find-wordbreak get-admin
             get-style-list get-padding)
  
    ;; helpers for on-paint
    (define/private (setup-dc dc)
      (let* ([font (send (get-style) get-font)]
             [font-size (send font get-point-size)])
        (send dc set-pen (get-foreground) 1 'solid)
        (send dc set-font (make-object font% font-size 'modern))
        (send dc set-text-foreground (get-foreground))))

    (define/private (get-style)
      (let ([style-list (get-style-list)])
        (or (send style-list find-named-style "Standard")
            (send style-list basic-style))))

    (define/private (get-foreground)
      (send (get-style) get-foreground))

    (define/private (save-dc-state dc)
      (set! saved-dc-state 
        (dc-state (send dc get-pen)
                  (send dc get-font)
                  (send dc get-text-foreground))))

    (define/private (restore-dc-state dc)
      (send dc set-pen (dc-state-pen saved-dc-state))
      (send dc set-font (dc-state-font saved-dc-state))
      (send dc set-text-foreground (dc-state-fg saved-dc-state)))

    ;; set up the clipping region to exclude where we'd like to draw
    (define/private (fix-clipping dc left top right bottom dx dy)
      (set! old-clipping (send dc get-clipping-region))
      (define mode-space   (make-object region% dc))
      (define new-clipping (make-object region% dc))
      (define everything   (make-object region% dc))
      (define-values (x y w h padding) 
        (get-mode-dimensions dc left top right bottom dx dy))
      (send mode-space set-rectangle x y (+ w (* 2 padding)) (+ h (* 2 padding)))
      (send everything set-rectangle
            (+ dx left) (+ dy top)
            (- right left) (- bottom top))
      (if old-clipping
          (send new-clipping union old-clipping)
          (send new-clipping union everything))
      (send new-clipping subtract mode-space)
      (send dc set-clipping-region new-clipping))

    ;; drawing mode lines
    ;; (is-a?/c dc<%>) real? real? real? real? real? real? -> void?
    (define/private (draw-mode-line dc left top right bottom dx dy)
      (define-values (x y width height padding)
        (get-mode-dimensions dc left top right bottom dx dy))
      (if (eq? mode 'command)
          (void)
          ;(send dc draw-rectangle (+ x padding) (+ y padding) width height)
          (send dc draw-text (mode-string) (+ x padding) (+ y padding))))

    ;; get the dimensions for how to draw a mode line
    ;; (is-a?/c dc<%>) real? real? real? real? real? real? ->
    ;;   (values x y width height padding)
    ;; where x, y specify top-left corner of mode line
    ;;       width, height are as named
    ;;       padding is the space to add around the text
    (define/private (get-mode-dimensions dc left top right bottom dx dy)
      (define-values (bx by bw bh) (values (box 0) (box 0) (box 0) (box 0)))
      ;(define-values (pl pt pr pb) (get-padding))
      (let ([admin (get-admin)])
        (when admin (send admin get-view bx by bw bh #f))
        (define mode-string (symbol->string mode))
        (define-values (_1 th _3 _4)
          (send dc get-text-extent mode-string))
        (define y (+ (unbox by) (- (unbox bh) th) dy))
        (values (+ (unbox bx) dx)
                (- (+ (unbox by) (- (unbox bh) th) dy) mode-padding)
                (unbox bw)
                th
                mode-padding)))

    ;; mode string for mode line
    ;; -> string?
    (define/private (mode-string)
      (string-upcase (format "-- ~a --" (symbol->string mode))))

    ;; handles a multi-character command
    ;; (is-a?/c key-event%) -> void?
    (define/private (do-command event)
      (let/cc exit
        (let ([key (send event get-key-code)])
          (cond [(eq? key 'escape) (clear-command)]
                [command-continuation (command-continuation event)]
                [else (match (send event get-key-code)
                        [#\d (do-delete (suspend exit))]
                        [#\y (do-yank (suspend exit))]
                        [_   (do-simple-command event)])]))))

    ;; handle deletes
    (define/private (do-delete event)
      (let ([deleter (lambda (s e) (send this kill 0 s e))])
        (match (send event get-key-code)
          [#\w (do-word deleter)]
          [#\d (do-line deleter)])))
   
    ;; handle yanking
    (define/private (do-yank event)
      (let ([copier (lambda (s e) (send this copy #f 0 s e))])
        (match (send event get-key-code)
          [#\w (do-word copier)]
          [#\y (do-line copier)])))

    (define-syntax-rule (do-line f)
      (let ([b (box 0)])
        (get-position b)
        (define line (position-line (unbox b)))
        (f (line-start-position line)
           (line-end-position line))))

    (define-syntax-rule (do-word f)
      (let ([start (box 0)]
            [end (box 0)])
        (get-position start)
        (get-position end)
        (find-wordbreak start end 'selection)
        (f (unbox start) (unbox end))))

    ;; suspend the current key handling and wait
    (define/private (suspend exit)
      (let/cc k
        (set! command-continuation
          (lambda (arg)
            (set! command-continuation #f)
            (k arg)))
        (exit (void))))

    ;; clear the command continuation
    (define/private (clear-command)
      (set! command-continuation #f))

    ;; (is-a?/c key-event%) -> void?
    (define/private (do-insert event)
      (if (eq? (send event get-key-code) 'escape)
          (set-mode! 'command)
          (super on-local-char event)))

    ;; (is-a?/c key-event%) -> void?
    (define/private (do-simple-command event)
      (match (send event get-key-code)
        ;; insertion
        [#\a (set-mode! 'insert)]
        [#\A (begin (move-position 'right #f 'line)
                    (set-mode! 'insert))]
        [#\i (set-mode! 'insert)]
        [#\I (begin (move-position 'left #f 'line)
                    (set-mode! 'insert))]
        ;; modes
        [#\v (set-mode! 'visual)]
        ;; movement
        [#\f (and (send event get-control-down)
                  (move-position 'down #f 'page))]
        [#\b (and (send event get-control-down)
                  (move-position 'up #f 'page))]
        [#\h (move-position 'left)]
        [#\j (move-position 'down)]
        [#\k (move-position 'up)]
        [#\l (move-position 'right)]
        [#\w (move-position 'right #f 'word)]
        [#\b (move-position 'right #f 'word)]
        ;; copy & paste & editing
        [#\D (delete-until-end)]
        [#\p (paste)]
        [#\u (undo)]
        [#\r (and (send event get-control-down)
                  (redo))]
        [_   (void)]))

    ;; (is-a?/c key-event%) -> void?
    (define/private (do-visual event)
      (match (send event get-key-code)
        ;; modes
        ['escape (set-mode! 'command)]
        ;; copy & paste
        [#\d (visual-kill)]
        [#\x (visual-kill)]
        [#\y (visual-copy)]
        [#\p (begin (paste)
                    (set-mode! 'command))]
        ;; visual movement
        [#\h (move-position 'left #t)]
        [#\j (move-position 'down #t)]
        [#\k (move-position 'up #t)]
        [#\l (move-position 'right #t)]
        ;; re-indent on tab
        [#\tab (super on-local-char event)]
        [_   (void)]))

    ;; -> void?
    (define/private (delete-until-end)
      (let* ([b (box 0)]
             [_ (get-position b)]
             [line (position-line (unbox b))]
             [eol (line-end-position line)])
        (kill 0 (unbox b) eol)))

    ;; copy selection
    (define/private (visual-copy)
      (let ([bs (box 0)]
            [be (box 0)])
        (get-position bs be)
        (copy #f 0 (unbox bs) (unbox be))
        (visual-cleanup)))

    ;; kill selection
    (define/private (visual-kill)
      (let ([bs (box 0)]
            [be (box 0)])
        (get-position bs be)
        (kill 0 (unbox bs) (unbox be))
        (visual-cleanup)))

    ;; clear selection and end visual mode
    (define/private (visual-cleanup)
      (let ([b (box 0)])
        (get-position b)
        (set-position (unbox b) 'same)
        (set-mode! 'command)))

    (super-new)))
