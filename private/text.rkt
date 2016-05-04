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

      ;; This variable tracks a vim-mode position that is separate from the
      ;; "position" that is tracked by the underlying editor. This allows, for example,
      ;; the selection to be separate from the cursor position in visual mode. It
      ;; also allows the vim plugin to set the position depending on the current character,
      ;; e.g., to cooperate with parenthesis matching.
      ;;
      ;; This position is often the same as the underlying text position though.
      (define vim-position 0)

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

      ;; called to update the mode and do any transition work between modes
      (define/private (set-mode! new-mode)
        (define old-mode mode)
        (set! mode new-mode)
        (when (eq? new-mode 'visual-line) 
          (set! visual-line-mode-direction 'same)
          (vim-move-position 'left #f 'line)
          (vim-move-position 'right #t 'line))
        (when (and (eq? new-mode 'visual)
                   (= (get-start-position)
                      (get-end-position)))
          ;; extend selection when entering visual mode to avoid having
          ;; nothing selected initially
          (vim-move-position 'right #t))
        (when (eq? new-mode 'search)
          (set! search-queue (make-queue)))
        (when (and (eq? new-mode 'command)
                   (eq? old-mode 'insert))
          (set! vim-position (get-start-position))
          (unless (at-start-of-line?)
            (vim-move-position 'left)))
        (update-mode!)
        (do-caret-update)
        (when (eq? new-mode 'insert)
          (set-position vim-position)
          (hide-caret #f)
          (flash-off)))

      ;; called to set the vim position, needed to make sure GUI updates are done
      ;; after a position is set (since `after-set-position` is not called for this)
      (define (set-vim-position! pos)
        (set! vim-position pos)
        (do-caret-update))

      ;; handle the GUI portion of setting the mode line
      (define/private (update-mode!)
        (send parent-frame set-vim-status-message (mode-string)))

      (define mode-padding 3)

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
        (do-caret-update))

      (define/private (do-caret-update)
        (cond [(and (not (empty-line?))
                    (not (at-end-of-line?))
                    (not (eq? mode 'insert)))
               (hide-caret #f)
               ;; highlight the cursor position like vim via (very long) flash
               (flash-off)
               (flash-on vim-position (add1 vim-position) #f #t 500000000)]
              [(not (eq? mode 'insert))
               (hide-caret #t)
               (invalidate-bitmap-cache)]
              [else (void)]))

      ;; override painting to draw an extra selection at the end of the line
      ;; like vim does.
      (define/override (on-paint before? dc left top right bottom
                                 dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (when (and (vim?)
                   (eq? mode 'command)
                   (not before?))
          (define cur-line (position-line vim-position))
          (when (= (line-end-position cur-line) vim-position)
            (define-values (x y) (values (box #f) (box #f)))
            (position-location vim-position x y #t #f #t)
            (define-values (x-val y-val)
              (values (+ dx (unbox x)) (+ dy (unbox y))))
            (when (and (<= left x-val right)
                       ;; the y-coord gets larger as it goes to the bottom
                       (>= bottom y-val top))
              (define y-bottom (box #f))
              (position-location vim-position #f y-bottom #f #f #t)
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
               tabify-selection

               ;; from color:text<%>
               skip-whitespace)

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
        (let ([key-code (send event get-key-code)])
          (or (eq? key-code 'escape)
              (and (send event get-control-down)
                   (or (eq? key-code #\c)
                       (eq? key-code #\[))))))

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
           (set-position (skip-whitespace-forward))]
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
          ['change-line
           (do-delete-line)
           (set-mode! 'insert)]
          ['change-rest
           (delete-until-end)
           (set-mode! 'insert)
           (unless (at-end-of-line?)
             (move-position 'right))]

          ;; modes
          ['visual      (set-mode! 'visual)]
          ['visual-line (set-mode! 'visual-line)]
          ['ex          (set-mode! 'ex)]

          ;; movement
          ['left          (vim-move-position 'left)]
          ['down          (vim-move-position 'down)]
          ['up            (vim-move-position 'up)]
          ['right         (vim-move-position 'right)]
          ['next-page     (vim-move-position 'down #f 'page)]
          ['previous-page (vim-move-position 'up #f 'page)]
          ['next-word     (vim-move-position 'right #f 'word)
                          (set-vim-position! (skip-whitespace-forward))]
          ['previous-word (vim-move-position 'left #f 'word)]
          ['continue
           (define-values (start end) (get-current-line-start-end))
           (cond [(and (or (= (sub1 end) vim-position) (empty-line?))
                       ;; only move if we're not about to hit the end or
                       ;; the next (and last) line is blank
                       (or (not (= (add1 vim-position) (last-position)))
                           (equal? #\newline
                                   (get-character vim-position))))
                  (vim-move-position 'down)
                  (vim-move-position 'left #f 'line)]
                 [else
                  (vim-move-position 'right)])]
          ['start-of-line         (vim-move-position 'left #f 'line)]
          ['end-of-line           (vim-move-position 'right #f 'line)]
          ;; FIXME: this is not quite right
          ['start-of-line-content (vim-move-position 'left #f 'line)]
          ['match (do-matching-paren
                    (λ (dir s e)
                      (match dir
                        ['backward (set-vim-position! s)]
                        ['forward  (set-vim-position! (sub1 e))])))]
          ['start-of-file (vim-move-position 'home #f)]
          ['end-of-file   (vim-move-position 'end #f)]

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
          ['delete-line (do-delete-line)]
          ['yank-line   (do-line (λ (s e) (send this copy #f 0 s e)))]
          ['paste       (do-paste)]
          ['undo        (undo)]
          ['redo        (redo)]

          ;; search
          ['search      (set-mode! 'search)]
          ['next-search (do-next-search #t)]
          ['prev-search (do-previous-search)]

          [_   (void)]))

      (define/private (handle-motion-command command)
        (match-define (motion-command operation motion) command)
        (match operation
          ['change (handle-motion motion
                                  (λ (s e) (send this kill 0 s e))
                                  void
                                  (λ () (set-mode! 'insert)))]
          ['delete (handle-motion motion
                                  (λ (s e) (send this kill 0 s e))
                                  void
                                  (λ () (adjust-caret-eol)))]
          ['yank   (handle-motion motion
                                  (λ (s e) (send this copy #f 0 s e))
                                  (λ () (set! paste-type 'normal))
                                  void)]))

      ;; motion (-> pos pos void) (-> void) (-> void) -> void
      ;; Abstracted handling of motion commands. Pass in argument
      ;; functions to customize it appropriately for specific commands.
      (define/private (handle-motion motion do-range do-pre do-post)
        (do-pre)
        (define ok?
          (match motion
            ['a-word (do-a-word do-range)]
            ['word-forward (do-word-forward do-range)]
            ['word-backward (do-word-backward do-range)]
            ['match (do-matching-paren
                      (λ (_ s e) (and s e (do-range s e))))]
            ['left  (do-character do-range 'backward)]
            ['down  (do-one-line do-range 'down)]
            ['up    (do-one-line do-range 'up)]
            ['right (do-character do-range)]))
        (when ok?
          (do-post)))

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
               (define old-pos vim-position)
               (define line (position-line old-pos))
               (define end (line-end-position line))
               (set-position (add1 old-pos))
               (cond [;; caret is as far right as it can go in command
                      (= (sub1 end) vim-position)
                      (begin-edit-sequence)
                      (insert " " end) ; dummy character, gets deleted
                      (set-position (add1 old-pos))
                      (paste)
                      (delete (line-end-position line))
                      (adjust-caret-eol)
                      (end-edit-sequence)]
                     [(paste)
                      ;; vim stays at the end of the paste, not right after
                      (set-position (sub1 vim-position))])]))

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
             (set-vim-position! mark-pos))]
          ['save-mark (set-mark mark)]))

      ;; Look up a mark and return the mapped position. If the
      ;; key is an invalid mark character, return #f
      (define/private (lookup-mark key)
        (vector-ref local-marks
                    (- (char->integer key)
                       (char->integer #\a))))

      ;; Set a mark for the current position
      (define/private (set-mark char)
        (vector-set! local-marks
                     (- (char->integer char)
                        (char->integer #\a))
                     vim-position))

      (define/private (do-line f)
        (if (= vim-position (last-position))
            (set! paste-type 'line-end)
            (set! paste-type 'line))
        (define line (position-line vim-position))
        (define start (line-start-position line))
        (f (if (and (= line (last-line))
                    (not (zero? line)))
               (sub1 start)
               start)
           (add1 (line-end-position line))))

      (define (do-delete-line)
        (do-line (λ (s e)
                   (send this kill 0 s e)
                   (vim-move-position 'left #f 'line))))

      (define (do-a-word f)
        (let ([start (box vim-position)]
              [end (box vim-position)])
          (find-wordbreak start end 'selection)
          (define start-pos (unbox start))
          (define end-pos (unbox end))
          (cond [;; whitespace before word and not the first word
                 (let ([bpos (skip-whitespace-backward start-pos)])
                   (and (not (= bpos start-pos))
                        (= (position-line bpos) (position-line start-pos))
                        bpos))
                 =>
                 (λ (bpos) (f bpos end-pos))]
                [;; whitespace after word up to end of line/word
                 (let ([fpos (skip-whitespace-forward end-pos)])
                   (and (not (= fpos end-pos))
                        (= (position-line fpos) (position-line end-pos))
                        fpos))
                 =>
                 (λ (fpos) (f start-pos fpos))]
                [;; otherwise do f with just the word
                 (f start-pos end-pos)])))

      ;; (position position -> any) -> any
      ;; handle a word forward motion, using f as the action
      (define (do-word-forward f)
        (define-values (start end)
          (values (box vim-position) (box vim-position)))
        (find-wordbreak start end 'selection)
        (f (get-start-position)
           ;; vim includes whitespace up to next word
           (skip-whitespace-forward (unbox end))))

      ;; (position position -> any) -> any
      ;; handle a word backward motion, using f as the action
      (define (do-word-backward f)
        (and (not (at-start-of-line?))
             (let ()
               (begin-edit-sequence)
               (define orig vim-position)
               (vim-move-position 'left #f 'word)
               (define word-start vim-position)
               (set-vim-position! orig)
               (f word-start orig)
               (end-edit-sequence))))

      (define (do-character f [dir 'forward])
        (cond [(eq? dir 'forward)
               (f vim-position (+ 1 vim-position))]
              [(and (eq? dir 'backward)
                    (not (at-start-of-line?)))
               (f (- vim-position 1) vim-position)]
              [else #f]))

      (define/private (do-one-line f [dir 'up])
        (define cur-line (position-line vim-position))
        (cond [(and (eq? dir 'up)
                    (>= (sub1 cur-line) 0))
               (f (line-start-position (sub1 cur-line))
                  (line-end-position cur-line))]
              [(and (eq? dir 'down)
                    (<= (add1 cur-line) (last-line)))
               (f (line-start-position cur-line)
                  (line-end-position (add1 cur-line)))]
              [else #f]))

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
          (kill 0 vim-position (add1 vim-position))
          (adjust-caret-eol)))

      ;; like the move-position method in texts, but this method adjusts both
      ;; the vim position and the text position
      (define/private (vim-move-position code [extend? #f] [kind 'simple])
        (begin-edit-sequence)
        (define-values (text-start text-end)
          (values (get-start-position) (get-end-position)))
        ;; since we use text's move-position to figure out how to move, first line
        ;; up the vim/text positions and then do a move
        (set-position vim-position)
        (move-position code extend? kind)
        (set-vim-position! (get-start-position))
        ;; Don't allow navigation to the "end of line" position
        ;; since this would go "off the end" in vim
        (when (eq? mode 'command)
          (when (and (not (empty-line?))
                     (at-end-of-line?))
            (set-vim-position! (sub1 vim-position))))
        ;; now handle how we reset the text position
        (cond [(= text-start text-end)
               ;; if the selection was a single position to begin with, we update
               ;; position based on the current character
               (define char (get-character vim-position))
               (when (equal? char #\))
                 (move-position 'right))]
              [else
               ;; if the selection was a range, then restore it
               (set-position text-start text-end)])
        (do-caret-update)
        (end-edit-sequence))

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
            (vim-move-position 'right)
            ;; compensate for insertion moving right
            (vim-move-position 'left))
        (end-edit-sequence))

      ;; (is-a?/c key-event%) -> void?
      (define/private (do-visual event)
        (match (send event get-key-code)
          [#\b (vim-move-position 'left #t 'word)]
          [#\w (vim-move-position 'right #t 'word)]
          [#\$ (vim-move-position 'right #t 'line)]
          [#\^ (vim-move-position 'left #t 'line)]
          [(or #\h 'left) (vim-move-position 'left #t)]
          [(or #\j 'down) (vim-move-position 'down #t)]
          [(or #\k 'up) (vim-move-position 'up #t)]
          [(or #\l 'right) (vim-move-position 'right #t)]
          [_ (do-visual-line event)]))

      (define/private (fill-line s e)
        (define-values (s* e*) (values (get-start-position) (get-end-position)))
        (cond [(= (position-line s*) (position-line e*))
               (set! visual-line-mode-direction 'same)
               (vim-move-position 'left #f 'line)
               (vim-move-position 'right #t 'line)]
              [(equal? visual-line-mode-direction 'up)
               (vim-move-position 'left #t 'line)]
              [(equal? visual-line-mode-direction 'down)
               (vim-move-position 'right #t 'line)]))

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
               (vim-move-position 'left #f 'line)
               (vim-move-position 'right #t 'line))
             (vim-move-position 'down #t)
             (fill-line s e)]
            [(or #\k 'up)
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'up)
               (vim-move-position 'right #f 'line)
               (vim-move-position 'left #t 'line))
             (vim-move-position 'up #t)
             (fill-line s e)]
            ['prior
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'up)
               (vim-move-position 'right #f 'line)
               (vim-move-position 'left #t 'line))
             (vim-move-position 'up #t 'page)
             (fill-line s e)]
            ['next
             (when (equal? visual-line-mode-direction 'same)
               (set! visual-line-mode-direction 'down)
               (vim-move-position 'left #f 'line)
               (vim-move-position 'right #t 'line))
             (vim-move-position 'down #t 'page)
             (fill-line s e)]
            ;; re-indent on tab
            [#\tab (super on-local-char event)]
            [_   (void)])]))


      ;; searching
      (inherit set-searching-state
               get-search-hit-count
               get-replace-search-hit
               get-search-bubbles
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

      ;; [position] -> void
      ;; execute a search going backwards from start-pos
      (define/private (do-previous-search [start-pos (get-start-position)])
        (when search-string
          (define bubbles (get-search-bubbles))
          ;; ASSUMPTION: bubbles are ordered by position
          (define matching-bubble
            (for/last ([bubble (in-list bubbles)]
                       #:when (< (caar bubble) start-pos))
              bubble))
          (cond [(null? bubbles) (void)]
                [matching-bubble
                 (set-position (caar matching-bubble))]
                [;; there are other search hits but there wasn't
                 ;; a match, therefore we have to loop from the end
                 else
                 (do-previous-search (last-position))])))

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
          [(app string->number (? exact-positive-integer? num))
           (set-position (line-start-position (sub1 num)))]
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

      (define/private (skip-whitespace-forward [pos #f])
        (skip-whitespace (or pos vim-position)
                         'forward
                         #f))

      (define/private (skip-whitespace-backward [pos #f])
        (skip-whitespace (or pos vim-position)
                         'backward
                         #f))

      ;; implements the behavior of "%" and friends in vim
      (define/private (do-matching-paren action)
        (define pos vim-position)
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
        (define cur-line (position-line vim-position))
        (= (line-end-position cur-line)
           (+ offset vim-position)))

      ;; determine if the current position is at the start of the line
      (define/private (at-start-of-line?)
        (define cur-line (position-line vim-position))
        (= (line-start-position cur-line) vim-position))

      ;; determine if the current line is empty
      (define/private (empty-line?)
        (define cur-line (position-line vim-position))
        (= (line-end-position cur-line)
           (line-start-position cur-line)))

      ;; When in command mode and an edit has been made, we may have to adjust the
      ;; caret if we're at the end of the line. Call this function to adjust.
      (define/private (adjust-caret-eol)
        (when (and (not (empty-line?))
                   (at-end-of-line?))
          (vim-move-position 'left)))

      (super-new)
      (do-caret-update))))

