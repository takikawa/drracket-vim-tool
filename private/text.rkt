#lang racket/gui

(require data/gvector
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
  [vim-emulation<%> interface?]
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
                  [on-local-char on-local-char/c])
        [vim? (->m boolean?)]
        [toggle-vim! (->m void?)]))])

(define/contract vim-prompt-tag
  (prompt-tag/c (-> (is-a?/c key-event%) any))
  (make-continuation-prompt-tag))

(define vim-emulation<%>
  (interface () vim?  toggle-vim!))

(define vim-emulation-mixin
  (λ (cls)
    (class* cls (vim-emulation<%>)

      ;; ==== public state & accessors ====
      (inherit get-tab invalidate-bitmap-cache)

      (define/public-final (vim?) vim-emulation?)

      (define/public-final (toggle-vim!)
        (preferences:set 'drracket:vim-emulation? (not vim-emulation?))
        (set! vim-emulation? (not vim-emulation?)))

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
          (move-position 'left #f 'line)
          (move-position 'right #t 'line))
        (when (eq? new-mode 'search)
          (set! search-queue (make-queue)))
        (when (and (eq? new-mode 'command)
                   (eq? old-mode 'insert))
          (adjust-caret-eol))
        (update-mode!))

      ;; handle the GUI portion of setting the mode line
      (define/private (update-mode!)
        (define frame (send (get-tab) get-frame))
        (send frame set-vim-status-message (mode-string)))

      (define vim-emulation? (preferences:get 'drracket:vim-emulation?))

      (define mode-padding 3)

      ;; use cmdline-style caret rendering as opposed to the GUI vim
      ;; style which uses a caret like "I" when in insert mode
      (define cmdline-caret? #t)

      ;; continuation into key handling routine
      (define key-cont #f)

      ;; paste-type : (or/c 'normal 'line)
      ;; Controls how pasting should behave based on how the copy was done
      ;; FIXME: once paste buffers are supported, this should map buffers
      ;;        to paste types instead
      (define paste-type 'normal)

      ;; ==== overrides & augments ====
      (inherit flash-on flash-off line-length hide-caret
               position-location get-line-spacing)

      ;; override character handling and dispatch based on mode
      ;; is-a?/c key-event% -> void?
      (define/override (on-local-char event)
        (if (and vim-emulation?
                 (not (ignored-event? event)))
            (if key-cont
                (key-cont event)
                (call/prompt
                 (λ ()
                   (cond [(eq? mode 'command) (do-command event)]
                         [(eq? mode 'insert)  (do-insert event)]
                         [(eq? mode 'visual)  (do-visual event)]
                         [(eq? mode 'visual-line) (do-visual-line event)]
                         [(eq? mode 'search) (do-search event)]
                         [(eq? mode 'ex) (do-ex event)]
                         [else (error "Unimplemented mode")])
                   (clear-cont!))
                 vim-prompt-tag
                 (λ (k) (set! key-cont k))))
            (super on-local-char event)))

      ;; some events are ignored because they're irrelevant for vim emulation,
      ;; such as key release events (FIXME: this may not be exhaustive)
      (define/private (ignored-event? event)
        (eq? (send event get-key-code) 'release))

      ;; override these for manual caret handling
      (define/augment (after-insert start end)
        (inner (void) after-insert start end)
        (do-caret-update))

      (define/augment (after-delete start end)
        (inner (void) after-delete start end)
        (adjust-caret-eol)
        (do-caret-update))

      (define/augment (after-set-position)
        (inner (void) after-set-position)
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
        (do-caret-update))

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
                 (if (and (= start-val end-val))
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
        (when (and cmdline-caret? (not before?))
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

      ;; ==== private functionality ====
      (inherit get-position set-position
               get-start-position get-end-position
               move-position
               copy paste kill undo redo delete insert
               line-start-position line-end-position position-line
               last-line
               local-to-global find-wordbreak
               begin-edit-sequence end-edit-sequence
               get-character find-newline
               forward-sexp backward-sexp)

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
        (call/comp (λ (k) (abort/cc vim-prompt-tag k))))

      ;; handles a multi-character command
      ;; (is-a?/c key-event%) -> void?
      (define/private (do-command event)
        (define key (send event get-key-code))
        (match key
          ['escape (clear-cont!)]
          [#\d (do-delete (get-next-key))]
          [#\y (do-yank (get-next-key))]
          [#\g (do-global (get-next-key))]
          [#\m (do-mark 'save (get-next-key))]
          [#\' (do-mark 'apostrophe (get-next-key))]
          [#\` (do-mark 'backtick (get-next-key))]
          [#\G (move-position 'end #f)]
          [(? (conjoin char? char-numeric?) digit) (do-repeat digit)]
          [_   (do-simple-command event)]))

      ;; handles global commands
      (define/private (do-global event)
        (match (send event get-key-code)
          ['release (do-global (get-next-key))]
          [#\g (move-position 'home #f)]
          [_ (clear-cont!)]))

      ;; handles command repetition and line jump
      (define/private (do-repeat digit)
        (define (char-numeric->number x) (string->number (string x)))
        (let loop ([num (char-numeric->number digit)])
          (match (send (get-next-key) get-key-code)
            [(or 'shift 'release) (loop num)]
            [#\G (if (zero? num)
                     (set-position (line-start-position (last-line)))
                     (set-position (line-start-position (sub1 num))))]
            [(? (conjoin char? char-numeric?) digit) (loop (+ (char-numeric->number digit) (* 10 num)))]
            [_ (clear-cont!)])))

      ;; handle deletes
      (define/private (do-delete event)
        (match (send event get-key-code)
          ['release (do-delete (get-next-key))]
          [#\w (do-word (λ (s e) (send this kill 0 s e)))]
          [#\d (do-line (λ (s e)
                          (send this kill 0 s e)
                          (send this move-position 'down)
                          (send this move-position 'left #f 'line)))]
          [_ (clear-cont!)]))

      ;; handle yanking
      (define/private (do-yank event)
        (set! paste-type 'normal)
        (let ([copier (lambda (s e) (send this copy #f 0 s e))])
          (match (send event get-key-code)
            ['release (do-yank (get-next-key))]
            [#\w (do-word copier)]
            [#\y (do-line copier)]
            [_ (clear-cont!)])))

      ;; handle pasting, esp. visual-line type pasting
      (define/private (do-paste)
        (cond [(eq? paste-type 'line)
               (begin-edit-sequence)
               (define end (get-end-position))
               (define line (position-line end))
               (define pos (line-end-position line))
               (insert "\n" pos)
               (paste 0 (add1 pos))
               (end-edit-sequence)]
              [else
               (paste)]))

      ;; handle mark setting and navigation
      (define/private (do-mark kind next-key)
        (define char (send next-key get-key-code))
        (when (mark-char? char)
          (match kind
            ['apostrophe
             (define mark-pos (lookup-mark char))
             (when mark-pos
               (define mark-line (position-line mark-pos))
               (set-position (line-start-position mark-line)))]
            ['backtick
             (define mark-pos (lookup-mark char))
             (when mark-pos
               (set-position mark-pos))]
            ['save (set-mark char)]))
        (clear-cont!))

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

      (define/private (mark-char? key)
        (and (char? key)
             (char>=? key #\a)
             (char<=? key #\z)))

      (define-syntax-rule (do-line f)
        (let ([b (box 0)])
          (set! paste-type 'line)
          (get-position b)
          (define line (position-line (unbox b)))
          (define start (line-start-position line))
          (f (if (zero? start) start (sub1 start))
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

      (define/private (do-delete-insertion-point)
        (define start (box #f))
        (get-position start)
        (delete (add1 (unbox start))))

      ;; (is-a?/c key-event%) -> void?
      (define/private (do-simple-command event)
        (match (send event get-key-code)
          ;; insertion
          [#\a (set-mode! 'insert)]
          [#\A (begin (set-mode! 'insert)
                      (move-position 'right #f 'line))]
          [#\i (set-mode! 'insert)]
          [#\I (begin (set-mode! 'insert)
                      (move-position 'left #f 'line))]
          [#\O (begin (set-mode! 'insert)
                      (insert-line-before)
                      (move-position 'up))]
          [#\o (begin (set-mode! 'insert)
                      (insert-line-after)
                      (move-position 'down))]
          ;; modes
          [#\v (set-mode! 'visual)]
          [#\V (set-mode! 'visual-line)]
          [#\: (set-mode! 'ex)]
          ;; movement
          [#\f (and (send event get-control-down)
                    (move-position 'down #f 'page))]
          [#\b (if (send event get-control-down)
                   (move-position 'up #f 'page)
                   (move-position 'left #f 'word))]
          [#\h (move-position 'left)]
          [#\j (move-position 'down)]
          [#\k (move-position 'up)]
          [#\l (move-position 'right)]
          [#\w (move-position 'right #f 'word)]
          [#\0 (move-position 'left #f 'line)]
          [#\$ (move-position 'right #f 'line)]
          [#\^ (move-position 'left #f 'line)]
          [#\% (move-matching-paren)]

          ;; editing
          [#\J (delete-next-newline-and-whitespace)]
          [#\x (do-delete-insertion-point)]
          ;; copy & paste & editing
          [#\D (delete-until-end)]
          [#\p (do-paste)]
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
          [#\b (move-position 'left #t 'word)]
          [#\w (move-position 'right #t 'word)]
          [#\$ (move-position 'right #t 'line)]
          [#\^ (move-position 'left #t 'line)]
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


      ;; searching
      ;; TODO: - backwards search
      ;;       - fix weird behavior when no more hits remain
      (inherit set-searching-state
               get-search-hit-count
               get-replace-search-hit
               search-updates-pending?)

      ;; (is-a?/c key-event%) -> void?
      ;; handle search mode key events
      (define/private (do-search event)
        (define key (send event get-key-code))
        (match key
          ['escape (set-mode! 'command)]
          [#\return
           (define the-string (search-queue->string))
           (set! search-string the-string)
           (do-next-search)
           (set-mode! 'command)]
          [#\backspace
           (unless (queue-empty? search-queue)
             (dequeue-char!))]
          [(? char?) (enqueue-char! key)]
          [_ (void)]))

      ;; (is-a?/c key-event%) -> void
      ;; handle ex commands
      (define/private (do-ex event)
        (define key (send event get-key-code))
        (match key
          [#\return (run-ex-command)]
          ['escape (set-mode! 'command)]
          [#\backspace
           (unless (= (gvector-count ex-queue) 1)
             (gvector-remove-last! ex-queue))]
          [(? char?) (gvector-add! ex-queue key)]
          [_ (void)])
        (update-mode!))

      ;; run the current ex command
      (define/private (run-ex-command)
        (match (list->string (gvector->list ex-queue))
          ["w" (send this save-file)]
          ["tabnext" (send (send (get-tab) get-frame) next-tab)]
          ["tabprev" (send (send (get-tab) get-frame) prev-tab)]
          [_ (void)])
        (set-mode! 'command)
        (set! ex-queue (gvector)))

      (define/private (do-next-search [start-at-next-word #t])
        (when search-string
          (define old-pos (box 0))
          (get-position old-pos)
          (when start-at-next-word
            (move-position 'right #f 'word))
          ;; set the search state to get the next hit
          (set-searching-state search-string #f #t #f)
          (let loop ()
            (when (search-updates-pending?)
              (yield)
              (loop)))
          (if (get-replace-search-hit)
              (set-position (get-replace-search-hit))
              (set-position (unbox old-pos)))
          ;; immediately clear the state to remove bubbles
          (set-searching-state #f #t #f #f)))

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
          (set-position newline-pos)
          (end-edit-sequence)))

      ;; implements the behavior of "%" in vim
      (define/private (move-matching-paren)
        (define pos-box (box 0))
        (get-position pos-box)
        (define pos (unbox pos-box))
        (define char (get-character pos))
        (match char
          [(or #\) #\] #\})
           (backward-sexp (add1 pos))]
          [(or #\( #\[ #\{)
           (forward-sexp pos)
           (move-position 'left)]
          [_ (void)]))

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
        (define start (get-start-position))
        (define end (get-end-position))
        (match dir
          ['down
           (define cur-line (position-line end))
           (define next-line (add1 cur-line))
           (set-position start (line-end-position next-line))]
          [_
           (define cur-line (position-line start))
           (define prev-line (sub1 cur-line))
           (when (>= prev-line 0)
             (set-position (line-start-position prev-line) end))]))

      ;; copy selection
      (define/private (visual-copy)
        (let ([bs (box 0)]
              [be (box 0)])
          (get-position bs be)
          (copy #f 0 (unbox bs)
                     (if (= (line-end-position (position-line (unbox be))) (unbox be))
                         (add1 (unbox be))
                         (unbox be)))
          (set! paste-type 'visual-line)
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
        (send this insert "\n" end))

      ;; insert line before the line the cursor is currently on
      (define/private (insert-line-before)
        (define-values (start _end) (get-current-line-start-end))
        (send this insert "\n" (if (zero? start) start (sub1 start))))

      ;; -> (values int int)
      ;; gets the start and end position of the line at the start of current selection
      (define/private (get-current-line-start-end)
        (define b (box 0))
        (get-position b)
        (define line (position-line (unbox b)))
        (values (line-start-position line)
                (line-end-position line)))

      ;; determine if the current position is at the end of the line
      (define/private (at-end-of-line?)
        (define-values (start end) (values (box #f) (box #f)))
        (get-position start end)
        (define cur-line (position-line (unbox start)))
        (= (line-end-position cur-line) (unbox start)))

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

