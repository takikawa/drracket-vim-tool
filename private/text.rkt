#lang racket/gui

(require framework)

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
        [toggle-vim! (->m void?)]))])

(define vim-emulation<%>
  (interface () vim?  toggle-vim!))

(define vim-emulation-mixin
  (mixin (text:basic<%> text:searching<%>) (vim-emulation<%>)

    ;; ==== public state & accessors ====
    (inherit invalidate-bitmap-cache)

    (define/public-final (vim?) vim-emulation?)

    (define/public-final (toggle-vim!)
      (preferences:set 'drracket:vim-emulation? (not vim-emulation?))
      (set! vim-emulation? (not vim-emulation?)))

    ;;; Private state

    ;; vim-style mode 
    ;; Editing modes: 'command 'insert 'visual
    ;; Bookkeeping: 'search
    (define mode 'command)
    
    ;; current search string as a list of chars
    (define search-string '())
    
    ;; listof<char?> -> void?
    (define/private (set-search-string! loc)
      (set! search-string loc)
      (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end))

    ;; -> string?
    (define/private (get-search-string)
      (list->string (reverse search-string)))

    (define/private (set-mode! new-mode)
      (set! mode new-mode)
      (when (eq? new-mode 'visual-line)
        (move-position 'left #f 'line)
        (move-position 'right #t 'line))
      (when (eq? new-mode 'search)
        (set-search-string! '()))
      (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end))

    (define vim-emulation? (preferences:get 'drracket:vim-emulation?))

    (define mode-padding 3)
    (define old-clipping #f)

    ;; save the dc state so we don't mangle it for others
    (define-struct dc-state (pen font fg))
    (define saved-dc-state #f)
    
    ;; continuation into key handling routine
    (define key-cont #f)

    ;; ==== overrides ====
    ;; override character handling and dispatch based on mode
    ;; is-a?/c key-event% -> void?
    (define/override (on-local-char event)
      (if vim-emulation?
          (if key-cont
              (key-cont event)
              (call-with-continuation-prompt
               (λ ()
                 (cond [(eq? mode 'command) (do-command event)]
                       [(eq? mode 'insert)  (do-insert event)]
                       [(eq? mode 'visual)  (do-visual event)]
                       [(eq? mode 'visual-line) (do-visual-line event)]
                       [(eq? mode 'search) (do-search event)]
                       [else (error "Unimplemented mode")])
                 (clear-cont!))
               (default-continuation-prompt-tag)
               (λ (k) (set! key-cont k))))
          (super on-local-char event)))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if (and vim-emulation?
               ;; in command mode, we don't draw anything special
               (not (eq? mode 'command)))
        (begin
          (save-dc-state dc)
          (setup-dc dc)
          (if (and before?)
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
      (cond [(eq? mode 'command) (void)]
            [else
              (send dc set-pen "white" 0 'transparent)
              (send dc draw-rectangle (+ x padding) (+ y padding) width height)
              (send dc draw-text (mode-string) (+ x padding) (+ y padding))]))

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
      (match mode
        ['search (get-search-string)]
        [_ (string-upcase (format "-- ~a --" (symbol->string mode)))]))
    
    ;; provide the next key later
    (define/private (get-next-key)
      (call-with-composable-continuation
       (λ (k)
         (abort-current-continuation
          (default-continuation-prompt-tag)
          (λ (v) (k v))))))

    ;; handles a multi-character command
    ;; (is-a?/c key-event%) -> void?
    (define/private (do-command event)
      (define key (send event get-key-code))
      (match key
        ['escape (clear-cont!)]
        [#\d (do-delete (get-next-key))]
        [#\y (do-yank (get-next-key))]
        [_   (do-simple-command event)]))

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

    ;; clear the command continuation
    (define/private (clear-cont!)
      (set! key-cont #f))

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
        [#\V (set-mode! 'visual-line)]
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
        ;; search
        [#\/ (set-mode! 'search)]
        [#\n (do-next-search)]
        [_   (void)]))
    
    ;; (is-a?/c key-event%) -> void?
    (define/private (do-visual event)
      (match (send event get-key-code)
        [#\h (move-position 'left #t)]
        [#\j (move-position 'down #t)]
        [#\k (move-position 'up #t)]
        [#\l (move-position 'right #t)]
        [_ (do-visual-line event)]))

    ;; (is-a?/c key-event%) -> void?
    (define/private (do-visual-line event)
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
        [#\j (visual-line-move 'down)]
        [#\k (visual-line-move 'up)]
        ;; re-indent on tab
        [#\tab (super on-local-char event)]
        [_   (void)]))
    
    (inherit set-searching-state
             get-replace-search-hit
             set-replace-start)
    
    ;; (is-a?/c key-event%) -> void?
    (define/private (do-search event)
      (define key (send event get-key-code))
      (match key
        ['escape (set-mode! 'command)]
        [#\return (set-searching-state (get-search-string) #f #f)
                  (do-next-search)
                  (set-mode! 'command)]
        [(? char?) (set-search-string! (cons key search-string))]
        [_ (void)]))
    
    (define/private (do-next-search [start-at-next-word #t])
      (when start-at-next-word
        (move-position 'right #f 'word))
      (define pos-box (box 0))
      (get-position pos-box)
      (set-replace-start (unbox pos-box))
      (when (get-replace-search-hit)
        (set-position (get-replace-search-hit))))

    ;; -> void?
    (define/private (delete-until-end)
      (let* ([b (box 0)]
             [_ (get-position b)]
             [line (position-line (unbox b))]
             [eol (line-end-position line)])
        (kill 0 (unbox b) eol)))
    
    ;; move selection by line
    ;; : (one-of/c 'down 'up) -> void?
    (define/private (visual-line-move dir)
      (move-position dir #t)
      (move-position 'right #t 'line))

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
