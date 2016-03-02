#lang racket/gui

;; The main GUI code for the DrRacket vim plugin
;;
;; How to add a new command:
;;   * Add parsing logic for the command in "commands.rkt" which
;;     will produce a representation of a command.
;;
;;   * Add handling code in the vim-emulation-mixin that dispatches
;;     on the command representation. (see methods that start with `handle-`)

(require "commands.rkt"
         data/gvector
         data/queue
         framework
         racket/control
         racket/function)

(define on-local-char/c
  (->m (is-a?/c key-event%) void?))

(define on-paint/c
  (->m any/c (is-a?/c dc<%>) real? real? real? real? real? real?
       (or/c (one-of/c 'no-caret 'show-inactive-caret 'show-caret)
             (cons/c natural-number/c natural-number/c))
       void?))

(provide/contract
  [vim-emulation-mixin
    (-> (class/c
          (inherit invalidate-bitmap-cache
                   get-position
                   set-position
                   move-position
                   insert
                   copy paste kill undo redo delete
                   line-start-position line-end-position position-line
                   get-view-size local-to-global
                   find-wordbreak get-admin
                   get-style-list get-padding
                   flash-on flash-off)
          (super on-local-char on-paint))
      (class/c
        [on-paint on-paint/c]
        [on-local-char on-local-char/c]
        (override [on-paint on-paint/c]
                  [on-local-char on-local-char/c])))])

(provide parent-frame)

(define vim-prompt-tag (make-continuation-prompt-tag))

(define-local-member-name parent-frame)

(define vim-emulation-mixin
  (λ (cls)
    (class cls

      ;; ==== public state & accessors ====
      (inherit invalidate-bitmap-cache)

      (define/public-final (vim?)
        (and (not (eq? parent-frame 'uninitialized))
             (send parent-frame vim?)))

      ;; this field should be initialized when the tab containing
      ;; this editor is created
      (field [parent-frame 'uninitialized])

      ;;; Private state

      ;; vim-style mode
      ;; Editing modes: 'command 'insert 'visual
      ;; Bookkeeping: 'search
      (define mode 'command)

      ;; used to build up a search string
      (define search-queue (make-queue))
      ;; current search string (#f means none set)
      (define search-string #f)

      ;; for ex commands
      (define ex-queue (gvector))

      ;; local mark store, a vector storing buffer positions
      ;; indexed by alphabet position, e.g., #\a -> 0
      ;; stores #f if the given mark isn't set
      (define local-marks (make-vector 26 #f))

      (define visual-line-mode-direction 'same)

      ;; helpers for searching
      ;; char? -> void?
      (define/private (enqueue-char! char)
        (enqueue-front! search-queue char)
        (update-mode!))

      ;; -> void?
      (define/private (dequeue-char!)
        (dequeue! search-queue)
        (update-mode!))

      ;; -> string?
      (define/private (search-queue->string)
        (list->string (reverse (queue->list search-queue))))

      (define/private (set-mode! new-mode)
        (define old-mode mode)
        (set! mode new-mode)
        (when (eq? new-mode 'visual-line) 
          (set! visual-line-mode-direction 'same)
          (move-position 'left #f 'line)
          (move-position 'right #t 'line))
        (when (and (eq? new-mode 'visual)
                   (= (get-start-position)
                      (get-end-position)))
          ;; extend selection when entering visual mode to avoid having
          ;; nothing selected initially
          (move-position 'right #t))
        (when (eq? new-mode 'search)
          (set! search-queue (make-queue)))
        (when (and (eq? new-mode 'command)
                   (eq? old-mode 'insert))
          (unless (at-start-of-line?)
            (move-position 'left)))
        (update-mode!)
        (do-caret-update))

      ;; handle the GUI portion of setting the mode line
      (define/private (update-mode!)
        (send parent-frame set-vim-status-message (mode-string)))

      (define mode-padding 3)

      ;; use cmdline-style caret rendering as opposed to the GUI vim
      ;; style which uses a caret like "I" when in insert mode
      (define cmdline-caret? #t)

      ;; continuation into key handling routine
      (define key-cont #f)

      ;; paste-type : (or/c 'normal 'line 'line-end)
      ;; Controls how pasting should behave based on how the copy was done
      ;; 'line-end corresponds to yanking lines including the end of the
      ;; buffer, which is missing a newline character
      ;; FIXME: once paste buffers are supported, this should map buffers
      ;;        to paste types instead
      (define paste-type 'normal)

      ;; ==== overrides & augments ====
      (inherit flash-on flash-off line-length hide-caret
               position-location get-line-spacing)

      ;; override character handling and dispatch based on mode
      ;; is-a?/c key-event% -> void?
      (define/override (on-local-char event)
        (if (and (vim?)
                 (not (ignored-event? event)))
            (call/prompt
             (λ ()
               (cond [key-cont (key-cont event)]
                     [(eq? mode 'command)
                      (define command
                        (parse-command event (λ () (get-next-key))))
                      (and command
                           (handle-command command))]
                     [(eq? mode 'insert)  (do-insert event)]
                     [(eq? mode 'visual)  (do-visual event)]
                     [(eq? mode 'visual-line) (do-visual-line event)]
                     [(eq? mode 'search) (do-search event)]
                     [(eq? mode 'ex) (do-ex event)]
                     [else (error "Unimplemented mode")])
               (clear-cont!))
             vim-prompt-tag
             (λ (k) (set! key-cont k)))
            (super on-local-char event)))

      ;; some events are ignored because they're irrelevant for vim emulation,
      ;; such as key release events (FIXME: this may not be exhaustive)
      (define/private (ignored-event? event)
        (eq? (send event get-key-code) 'release))

      ;; override these for manual caret handling
      (define/augment (after-insert start end)
        (inner (void) after-insert start end)
        (when (vim?)
          (do-caret-update)))

      (define/augment (after-delete start end)
        (inner (void) after-delete start end)
        (when (vim?)
          (do-caret-update)))

      (define/augment (after-set-position)
        (inner (void) after-set-position)
        (when (vim?)
          ;; Don't allow navigation to the "end of line" position when
          ;; in command mode, since this goes "off the end" in vim
          (when (eq? mode 'command)
            (define-values (start end) (values (box #f) (box #f)))
            (get-position start end)
            (define-values (start-val end-val) (values (unbox start) (unbox end)))
            (when (and (= start-val end-val)
                       (not (empty-line?))
                       (at-end-of-line?))
              (set-position (sub1 start-val) (sub1 end-val))))
          (do-caret-update)))

      (define/private (do-caret-update)
        (define-values (start end) (values (box #f) (box #f)))
        (get-position start end)
        (define-values (start-val end-val) (values (unbox start) (unbox end)))
        (cond [(and (not (empty-line?))
                    (not (at-end-of-line?)))
               ;; The use of hide-caret here and below for some reason causes
               ;; the "fake" caret drawn in the on-paint method below to contain
               ;; some extra blank space. So instead live with a real caret
               ;; being temporarily painted over our fake one for now.
               ;(hide-caret #f)
               ;; for a single character/item selection, try to highlight it
               ;; like vim will by offsetting by one
               (define start* start-val)
               (define end*
                 (if (and (= start-val end-val)
                          (not (equal? mode 'insert)))
                     (add1 end-val)
                     end-val))
               (flash-off)
               (flash-on start* end* #f #t 500000000)]
              [else
               ;(hide-caret #t)
               (invalidate-bitmap-cache)]))

      ;; override painting to draw an extra selection at the end of the line
      ;; like vim does.
      (define/override (on-paint before? dc left top right bottom
                                 dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (when (and (vim?) cmdline-caret? (not before?))
          (define-values (start end) (values (box #f) (box #f)))
          (get-position start end)
          (define-values (start-val end-val) (values (unbox start) (unbox end)))
          (define cur-line (position-line start-val))
          (when (and (= start-val end-val)
                     (= (line-end-position cur-line) start-val))
            (define-values (x y) (values (box #f) (box #f)))
            (position-location start-val x y #t #f #t)
            (define-values (x-val y-val)
              (values (+ dx (unbox x)) (+ dy (unbox y))))
            (when (and (<= left x-val right)
                       ;; the y-coord gets larger as it goes to the bottom
                       (>= bottom y-val top))
              (define y-bottom (box #f))
              (position-location start-val #f y-bottom #f #f #t)
              (define old-brush (send dc get-brush))
              (define old-pen (send dc get-pen))
              (define new-brush
                (new brush% [color (get-highlight-background-color)]))
              (define new-pen (new pen% [style 'transparent]))
              (send dc set-brush new-brush)
              (send dc set-pen new-pen)
              ;; assumes fixed-width font
              (define-values (width _2 _3 _4) (send dc get-text-extent "a"))
              (send dc draw-rectangle
                    x-val y-val
                    width
                    (- (- (+ dy (unbox y-bottom)) y-val)
                       (get-line-spacing)))
              (send dc set-brush old-brush)
              (send dc set-pen old-pen)))))

      ;; override position movement to enforce boundaries of command
      ;; mode movement
      (define/override (move-position code [extend? #f] [kind 'simple])
        (cond [(and (vim?) (eq? mode 'command))
               (define line (position-line (get-start-position)))
               (cond [(and (empty-line?)
                           (or (eq? code 'right) (eq? code 'left)))
                      (void)]
                     [(and (at-start-of-line?) (eq? code 'left))
                      (void)]
                     [(and (= line (last-line)) (eq? code 'down))
                      (void)]
                     [(and (zero? line) (eq? code 'up))
                      (void)]
                     [else (super move-position code extend? kind)])]
              [else (super move-position code extend? kind)]))

      ;; make sure mode is consistent when focus comes back
      (define/override (on-focus in?)
        (super on-focus in?)
        (when (and (vim?) in? (not (eq? parent-frame 'uninitialized)))
          (update-mode!)))

      ;; ==== private functionality ====
      (inherit get-position set-position
               get-start-position get-end-position
               copy paste kill undo redo delete insert
               line-start-position line-end-position position-line
               last-line last-position
               local-to-global find-wordbreak
               begin-edit-sequence end-edit-sequence
               get-character find-newline
               get-forward-sexp get-backward-sexp
               tabify-selection)

      ;; mode string for mode line
      ;; -> string?
      (define/private (mode-string)
        (match mode
          ['command ""]
          ['search (string-append "/" (search-queue->string))]
          ['ex (string-append ":" (list->string (gvector->list ex-queue)))]
          [_ (string-upcase (format "-- ~a --" (symbol->string mode)))]))

      ;; provide the next key later
      (define/private (get-next-key)
        (call/comp (λ (k) (abort/cc vim-prompt-tag k))
                   vim-prompt-tag))

      ;; check whether an event is equivalent to "escape"
      (define/private (check-escape event)
        (or (eq? (send event get-key-code) 'escape)
            (and (equal? (send event get-key-code) #\c)
                 (send event get-control-down))))

      ;; handles command-mode operations
      ;; Command -> Void
      (define/private (handle-command command)
        (match command
          [(? motion-command?)   (handle-motion-command command)]
          [(? mark-command?)     (handle-mark command)]
          [(? replace-command?)  (handle-replace command)]
          [(? repeat-command?)  (for ([i (in-range (repeat-command-repeat command))])
                                  (handle-command (repeat-command-command command)))]
          [(? goto-command?)
           (match-define (goto-command line) command)
           (if (eq? line 'last-line)
               (set-position (line-start-position (last-line)))
               (set-position (line-start-position (sub1 line))))]
          [_ (handle-simple-command command)]))

      ;; handle a command with no motion/repeat
      (define/private (handle-simple-command command)
        (match command
          ;; insertion
          ['insert-end
           (set-mode! 'insert)
           (unless (at-end-of-line?)
             (move-position 'right))]
          ['insert-end-line
           (set-mode! 'insert)
           (move-position 'right #f 'line)]
          ['insert
           (set-mode! 'insert)]
          ['insert-line
           (set-mode! 'insert)
           (move-position 'left #f 'line)
           (skip-whitespace)]
          ['insert-previous-line
           (set-mode! 'insert)
           (insert-line-before)]
          ['insert-next-line
           (set-mode! 'insert)
           (define-values (_start end) (get-current-line-start-end))
           (insert-line-after)]
          ['insert-at-delete
           (do-delete-insertion-point)
           (set-mode! 'insert)]

          ;; modes
          ['visual      (set-mode! 'visual)]
          ['visual-line (set-mode! 'visual-line)]
          ['ex          (set-mode! 'ex)]

          ;; movement
          ['left          (move-position 'left)]
          ['down          (move-position 'down)]
          ['up            (move-position 'up)]
          ['right         (move-position 'right)]
          ['next-page     (move-position 'down #f 'page)]
          ['previous-page (move-position 'up #f 'page)]
          ['next-word     (move-position 'right #f 'word)]
          ['previous-word (move-position 'left #f 'word)]
          ['continue
           (define-values (start end) (get-current-line-start-end))
           (cond [(and (or (= (sub1 end) (get-start-position)) (empty-line?))
                       ;; only move if we're not about to hit the end or
                       ;; the next (and last) line is blank
                       (or (not (= (add1 (get-end-position)) (last-position)))
                           (equal? #\newline
                                   (get-character (get-end-position)))))
                  (move-position 'down)
                  (move-position 'left #f 'line)]
                 [else
                  (move-position 'right)])]
          ['start-of-line         (move-position 'left #f 'line)]
          ['end-of-line           (move-position 'right #f 'line)]
          ;; FIXME: this is not quite right
          ['start-of-line-content (move-position 'left #f 'line)]
          ['match (do-matching-paren
                    (λ (dir s e)
                      (match dir
                        ['backward (set-position s)]
                        ['forward  (set-position (sub1 e))])))]
          ['start-of-file (move-position 'home #f)]
          ['end-of-file   (move-position 'end #f)]

          ;; editing
          ['join-line        (delete-next-newline-and-whitespace)]
          ['delete-at-cursor (do-delete-insertion-point)]

          ;; FIXME: in vim this can call out to an external program, but
          ;;        for now it only does the default behavior of indenting
          ['filter-line
           (define-values (start end) (get-current-line-start-end))
           (tabify-selection start end)]

          ;; copy & paste & editing
          ['delete-rest (delete-until-end)]
          ['delete-line (do-line (λ (s e)
                                   (send this kill 0 s e)
                                   (send this move-position 'left #f 'line)))]
          ['yank-line   (do-line (λ (s e) (send this copy #f 0 s e)))]
          ['paste       (do-paste)]
          ['undo        (undo)]
          ['redo        (redo)]

          ;; search
          ['search      (set-mode! 'search)]
          ['next-search (do-next-search #t)]

          [_   (void)]))

      (define/private (handle-motion-command command)
        (match-define (motion-command operation motion) command)
        (match operation
          ['delete (handle-delete motion)]
          ['yank   (handle-yank motion)]))

      ;; handle deletion based on a motion
      (define/private (handle-delete motion)
        (match motion
          ['word  (do-word (λ (s e) (send this kill 0 s e)))]
          ['match (do-matching-paren
                    (λ (_ s e) (and s e (send this kill 0 s e))))]
          ['right (do-character (λ (s e) (send this kill 0 s e)))])
        (adjust-caret-eol))

      ;; handle yanking based on a motion
      (define/private (handle-yank motion)
        (set! paste-type 'normal)
        (let ([copier (lambda (s e) (send this copy #f 0 s e))])
          (match motion
            ['word  (do-word copier)]
            ['match (do-matching-paren
                      (λ (_ s e) (and s e (copier s e))))]
            ['right (do-character copier)])))

      ;; handle pasting, esp. visual-line type pasting
      (define/private (do-paste)
        (cond [(or (eq? paste-type 'line)
                   (eq? paste-type 'line-end))
               (begin-edit-sequence)
               (define end (get-end-position))
               (define line (position-line end))
               (define num-lines (add1 (last-line)))
               (define pos (line-end-position line))
               ;; this insertion is needed to make the paste work
               (insert "\n" pos)
               (paste 0 (add1 pos))
               ;; Remove the extra "\n" if we are not at the end. We
               ;; retain it at the end because the last line is missing
               ;; a newline character.
               (unless (eq? paste-type 'line-end)
                 (define diff-lines (- (add1 (last-line)) num-lines))
                 (delete (line-start-position (+ line diff-lines))))
               (end-edit-sequence)]
              [else
               (define old-pos (get-start-position))
               (define line (position-line old-pos))
               (define end (line-end-position line))
               (set-position (add1 old-pos))
               (cond [;; caret is as far right as it can go in command
                      (= (sub1 end) (get-start-position))
                      (begin-edit-sequence)
                      (insert " " end) ; dummy character, gets deleted
                      (set-position (add1 old-pos))
                      (paste)
                      (delete (line-end-position line))
                      (adjust-caret-eol)
                      (end-edit-sequence)]
                     [(paste)
                      ;; vim stays at the end of the paste, not right after
                      (set-position (sub1 (get-start-position)))])]))

      ;; handle mark setting and navigation
      (define/private (handle-mark command)
        (match-define (mark-command kind mark) command)
        (match kind
          ['goto-mark-line
           (define mark-pos (lookup-mark mark))
           (when mark-pos
             (define mark-line (position-line mark-pos))
             (set-position (line-start-position mark-line)))]
          ['goto-mark-char
           (define mark-pos (lookup-mark mark))
           (when mark-pos
             (set-position mark-pos))]
          ['save-mark (set-mark mark)]))

      ;; Look up a mark and return the mapped position. If the
      ;; key is an invalid mark character, return #f
      (define/private (lookup-mark key)
        (vector-ref local-marks
                    (- (char->integer key)
                       (char->integer #\a))))

      ;; Set a mark for the current position
      (define/private (set-mark char)
        (define start-box (box 0))
        (get-position start-box)
        (vector-set! local-marks
                     (- (char->integer char)
                        (char->integer #\a))
                     (unbox start-box)))

      (define-syntax-rule (do-line f)
        (let ([b (box 0)])
          (if (= (get-end-position) (last-position))
              (set! paste-type 'line-end)
              (set! paste-type 'line))
          (get-position b)
          (define line (position-line (unbox b)))
          (define start (line-start-position line))
          (f (if (and (= line (last-line))
                      (not (zero? line)))
                 (sub1 start)
                 start)
             (add1 (line-end-position line)))))

      (define-syntax-rule (do-word f)
        (let ([start (box 0)]
              [end (box 0)])
          (get-position start)
          (get-position end)
          (find-wordbreak start end 'selection)
          (f (unbox start) (unbox end))))

      (define-syntax-rule (do-character f)
        (let ([start (box 0)]
              [end (box 0)])
          (get-position start)
          (get-position end)
          (f (unbox start) (+ 1 (unbox end)))))

      ;; clear the command continuation
      (define/private (clear-cont!)
        (set! key-cont #f))

      ;; (is-a?/c key-event%) -> void?
      (define/private (do-insert event)
        (if (check-escape event)
            (set-mode! 'command)
            (super on-local-char event)))

      (define/private (do-delete-insertion-point)
        (unless (empty-line?)
          (kill 0 (get-start-position) (add1 (get-start-position)))
          (adjust-caret-eol)))

      ;; ReplaceCommand -> Void
      ;; FIXME: make this work correctly for visual mode, etc.
      (define/private (handle-replace command)
        (match-define (replace-command char) command)
        (define pos (get-start-position))
        (define eol? (at-end-of-line? 1))
        (begin-edit-sequence)
        (do-delete-insertion-point)
        (insert char pos)
        (if eol?
            (move-position 'right)
            ;; compensate for insertion moving right
            (move-position 'left))
        (end-edit-sequence))

      ;; (is-a?/c key-event%) -> void?
      (define/private (do-visual event)
        (match (send event get-key-code)
          [#\b (move-position 'left #t 'word)]
          [#\w (move-position 'right #t 'word)]
          [#\$ (move-position 'right #t 'line)]
          [#\^ (move-position 'left #t 'line)]
          [(or #\h 'left) (move-position 'left #t)]
          [(or #\j 'down) (move-position 'down #t)]
          [(or #\k 'up) (move-position 'up #t)]
          [(or #\l 'right) (move-position 'right #t)]
          [_ (do-visual-line event)]))

      (define/private (fill-line s e)
        (define-values (s* e*) (values (get-start-position) (get-end-position)))
        (cond [(= (position-line s*) (position-line e*))
               (set! visual-line-mode-direction 'same)
               (move-position 'left #f 'line)
               (move-position 'right #t 'line)]
              [(equal? visual-line-mode-direction 'up)
               (move-position 'left #t 'line)]
              [(equal? visual-line-mode-direction 'down)
               (move-position 'right #t 'line)]))

      ;; (is-a?/c key-event%) -> void?
      (define/private (do-visual-line event)
        (define-values (s e) (values (get-start-position) (get-end-position)))
        (cond
         [(check-escape event) (set-mode! 'command)]
         [else
          (match (send event get-key-code)
            ;; copy & paste
            [#\d (visual-kill)]
            [#\x (visual-kill)]
            [#\y (visual-copy)]
            [#\p (begin (paste)
                        (set-mode! 'command))]
            ;; visual movement
            [(or #\j 'down)
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'down)
               (move-position 'left #f 'line)
               (move-position 'right #t 'line))
             (move-position 'down #t)
             (fill-line s e)]
            [(or #\k 'up)
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'up)
               (move-position 'right #f 'line)
               (move-position 'left #t 'line))
             (move-position 'up #t)
             (fill-line s e)]
            ['prior
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'up)
               (move-position 'right #f 'line)
               (move-position 'left #t 'line))
             (move-position 'up #t 'page)
             (fill-line s e)]
            ['next
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'down)
               (move-position 'left #f 'line)
               (move-position 'right #t 'line))
             (move-position 'down #t 'page)
             (fill-line s e)]
            ;; re-indent on tab
            [#\tab (super on-local-char event)]
            [_   (void)])]))


      ;; searching
      ;; TODO: - backwards search
      (inherit set-searching-state
               get-search-hit-count
               get-replace-search-hit
               finish-pending-search-work)

      ;; (is-a?/c key-event%) -> void?
      ;; handle search mode key events
      (define/private (do-search event)
        (define key (send event get-key-code))
        (cond
         [(check-escape event) (set-mode! 'command)]
         [else
          (match key
            ['escape (set-mode! 'command)]
            [#\return
             (define the-string (search-queue->string))
             (unless (= (string-length the-string) 0)
               (set! search-string the-string)
               (do-next-search))
             (set-mode! 'command)]
            [#\backspace
             (unless (queue-empty? search-queue)
               (dequeue-char!))]
            [(? char?) (enqueue-char! key)]
            [_ (void)])]))

      ;; [Boolean] -> Void
      (define/private (do-next-search [continuing? #f])
        (when search-string
          (define-values (_ total) (get-search-hit-count))
          (cond [(and continuing? (> total 0))
                 (begin-edit-sequence)
                 (move-position 'right)
                 (sleep/yield 0.1) ; timeout determined experimentally
                 (define-values (before _) (get-search-hit-count))
                 (cond ;; if there are more hits ahead in the buffer
                       ;; then continue to the next hit
                       [(> (- total before) 0)
                        (set-position (get-replace-search-hit))]
                       ;; there are hits before the cursor and none after
                       ;; so start over from the top
                       [(and continuing? (> before 0))
                        (set-position 0)
                        ;; keep waiting until the search updates
                        ;; ASSUMPTION: the buffer is not edited while we yield
                        (let loop ([hit (get-replace-search-hit)])
                          (if hit
                              (set-position hit)
                              (loop (begin (yield) (get-replace-search-hit)))))])
                 (end-edit-sequence)]
                ;; start a fresh search
                [else
                 (set-searching-state search-string #f #t #f)
                 (finish-pending-search-work)
                 (when (get-replace-search-hit)
                   (set-position (get-replace-search-hit)))])))

      ;; (is-a?/c key-event%) -> void
      ;; handle ex commands
      (define/private (do-ex event)
        (define key (send event get-key-code))
        (cond
         [(check-escape event)
          (set! ex-queue (gvector))
          (set-mode! 'command)]
         [else
          (match key
            [#\return (run-ex-command)]
            [#\backspace
             (cond [(= (gvector-count ex-queue) 0)
                    (set! ex-queue (gvector))
                    (set-mode! 'command)]
                   [else
                    (gvector-remove-last! ex-queue)])]
            [(? char?) (gvector-add! ex-queue key)]
            [_ (void)])])
        (update-mode!))

      ;; run the current ex command
      (define/private (run-ex-command)
        (match (list->string (gvector->list ex-queue))
          ["q" (send parent-frame close-current-tab)]
          ["w" (send this save-file)]
          ["tabnew" (send parent-frame open-in-new-tab #f)]
          ["tabnext" (send parent-frame next-tab)]
          ["tabprev" (send parent-frame prev-tab)]
          [(pregexp "tabm \\+(\\d+)" (list _ c)) (for ([i (in-range (string->number c))])
                                                   (send parent-frame move-current-tab-right))]
          [(pregexp "tabm \\-(\\d+)" (list _ c)) (for ([i (in-range (string->number c))])
                                                   (send parent-frame move-current-tab-left))]
          [_ (void)])
        (set-mode! 'command)
        (set! ex-queue (gvector)))

      ;; deletes starting from the next newline and to the first
      ;; non-whitespace character after that position
      (define/private (delete-next-newline-and-whitespace)
        (define newline-pos (find-newline))
        (when newline-pos
          (begin-edit-sequence)
          (delete newline-pos)
          (let loop ([char (get-character newline-pos)])
            (when (and (char-whitespace? char)
                       (not (eq? #\newline char)))
              (delete newline-pos)
              (loop (get-character newline-pos))))
          (insert #\space (sub1 newline-pos))
          (set-position newline-pos)
          (end-edit-sequence)))

      ;; move the cursor until whitespace is skipped
      (define/private (skip-whitespace)
        (let loop ([char (get-character (get-start-position))])
          (when (and (char-whitespace? char)
                     (not (eq? #\newline char)))
            (move-position 'right)
            (loop (get-character (get-start-position))))))

      ;; implements the behavior of "%" and friends in vim
      (define/private (do-matching-paren action)
        (define pos-box (box 0))
        (get-position pos-box)
        (define pos (unbox pos-box))
        (define char (get-character pos))
        (match char
          [(or #\) #\] #\})
           (define maybe-back (get-backward-sexp (add1 pos)))
           (when maybe-back
             (action 'backward maybe-back (add1 pos)))]
          [(or #\( #\[ #\{)
           (define maybe-fwd (get-forward-sexp pos))
           (when maybe-fwd
             (action 'forward pos maybe-fwd))]
          [_ (void)]))

      ;; -> void?
      (define/private (delete-until-end)
        (let* ([b (box 0)]
               [_ (get-position b)]
               [line (position-line (unbox b))]
               [eol (line-end-position line)])
          (kill 0 (unbox b) eol)
          (adjust-caret-eol)))

      ;; copy selection
      (define/private (visual-copy)
        (let ([bs (box 0)]
              [be (box 0)])
          (get-position bs be)
          (copy #f 0 (unbox bs)
                     (if (= (line-end-position (position-line (unbox be))) (unbox be))
                         (add1 (unbox be))
                         (unbox be)))
          (set! paste-type
                (if (eq? mode 'visual-line)
                    (if (= (get-end-position) (last-position))
                        'line-end
                        'line)
                    'normal))
          (visual-cleanup)))

      ;; kill selection
      (define/private (visual-kill)
        (let ([bs (box 0)]
              [be (box 0)])
          (get-position bs be)
          (kill 0 (unbox bs)
                  (if (= (line-end-position (position-line (unbox be))) (unbox be))
                      (add1 (unbox be))
                      (unbox be)))
          (visual-cleanup)))

      ;; clear selection and end visual mode
      (define/private (visual-cleanup)
        (let ([b (box 0)])
          (get-position b)
          (set-position (unbox b) 'same)
          (set-mode! 'command)))

      ;; insert line after the line the cursor is currently on
      (define/private (insert-line-after)
        (define-values (_start end) (get-current-line-start-end))
        (begin-edit-sequence)
        (set-position end)
        (send this insert-return)
        (end-edit-sequence))

      ;; insert line before the line the cursor is currently on
      (define/private (insert-line-before)
        (define-values (start _end) (get-current-line-start-end))
        (begin-edit-sequence)
        (set-position (if (zero? start) start (sub1 start)))
        (send this insert-return)
        (end-edit-sequence))

      ;; -> (values int int)
      ;; gets the start and end position of the line at the start of current selection
      (define/private (get-current-line-start-end)
        (define b (box 0))
        (get-position b)
        (define line (position-line (unbox b)))
        (values (line-start-position line)
                (line-end-position line)))

      ;; determine if the current position is at the end of the line
      ;; possibly counting an offset from the actual current position
      (define/private (at-end-of-line? [offset 0])
        (define-values (start end) (values (box #f) (box #f)))
        (get-position start end)
        (define cur-line (position-line (unbox start)))
        (= (line-end-position cur-line)
           (+ offset (unbox start))))

      ;; determine if the current position is at the start of the line
      (define/private (at-start-of-line?)
        (define-values (start end) (values (box #f) (box #f)))
        (get-position start end)
        (define cur-line (position-line (unbox start)))
        (= (line-start-position cur-line) (unbox start)))

      ;; determine if the current line is empty
      (define/private (empty-line?)
        (define-values (start end) (values (box #f) (box #f)))
        (get-position start end)
        (define cur-line (position-line (unbox start)))
        (= (line-end-position cur-line)
           (line-start-position cur-line)))

      ;; When in command mode and an edit has been made, we may have to adjust the
      ;; caret if we're at the end of the line. Call this function to adjust.
      (define/private (adjust-caret-eol)
        (when (and (not (empty-line?))
                   (at-end-of-line?))
          (move-position 'left)))

      (super-new)
      (do-caret-update))))

