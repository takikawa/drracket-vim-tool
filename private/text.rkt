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
         "utils.rkt"
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
                   get-style-list get-padding)
          (super on-local-char on-paint))
      (class/c
        [on-paint on-paint/c]
        [on-local-char on-local-char/c]
        (override [on-paint on-paint/c]
                  [on-local-char on-local-char/c])))])

(provide parent-frame)

(define vim-prompt-tag (make-continuation-prompt-tag))

(define-local-member-name parent-frame)

;; constants for coloring
(define cursor-color "slategray")
(define selection-color "lightsteelblue")

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

      ;; This variable should be updated whenever the cursor moves.
      ;; The cursor on up/down movements chooses the max of the text movement
      ;; position and this variable.
      (define current-column-position 0)

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

      ;; track last command for repeat command
      (define last-command #f)

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
      (inherit line-length hide-caret position-location get-line-spacing
               find-position global-to-local)

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
                     [(or (eq? mode 'visual)
                          (eq? mode 'visual-line))
                      (do-visual event)]
                     [(eq? mode 'search) (do-search event)]
                     [(eq? mode 'ex) (do-ex event)]
                     [else (error "Unimplemented mode")])
               (clear-cont!))
             vim-prompt-tag
             (λ (k) (set! key-cont k)))
            (super on-local-char event)))

      ;; override mouse handling
      (define/override (on-local-event event)
        (super on-local-event event)
        (when (vim?)
          (define type (send event get-event-type))
          (define pos
            (let ([x (box (send event get-x))]
                  [y (box (send event get-y))])
              (global-to-local x y)
              (find-position (unbox x) (unbox y))))
          ;; FIXME: in gvim a mouse drag in insert mode retains
          ;;        insert mode while doing visual mode too
          (cond [(eq? type 'left-down)
                 ;; clicks in visual mode escape the mode
                 (when (or (eq? mode 'visual)
                           (eq? mode 'visual-line))
                   (set-mode! 'command))
                 (set-vim-position! pos)
                 (set-position pos)
                 (do-caret-update)]
                [;; left click + drag
                 (and (eq? type 'motion)
                      (send event get-left-down)
                      (not (send event get-middle-down))
                      (not (send event get-right-down)))
                 (set! visual-line-mode-direction 'same)
                 ;; manually set up mode to avoid clobbering the position that the
                 ;; superclass mouse handler sets
                 (set! mode 'visual)
                 (hide-caret #t)
                 (update-mode!)
                 (do-caret-update)])))

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
          (do-caret-update)))

      ;; handle updating the drawing of the cursor/selection after movements
      ;; or other editing actions
      (define/private (do-caret-update)
        (unhighlight-ranges/key 'drracket-vim-highlight)

        ;; draw the cursor
        (cond [(and (not (empty-line?))
                    (not (at-end-of-line?))
                    (not (eq? mode 'insert)))
               (highlight-range vim-position (add1 vim-position) cursor-color
                                #f
                                'high
                                #:key 'drracket-vim-highlight)]
              [else
               (invalidate-bitmap-cache)])

        (when (or (eq? mode 'visual)
                  (eq? mode 'visual-line))
          (highlight-range (get-start-position)
                           (get-end-position)
                           selection-color
                           #f
                           'low ; to draw under the cursor
                           #:key 'drracket-vim-highlight)))

      ;; override painting to draw an extra selection at the end of the line
      ;; like vim does.
      (define/override (on-paint before? dc left top right bottom
                                 dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (when (and (vim?)
                   (not (eq? mode 'insert))
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
                (new brush% [color cursor-color]))
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

      ;; ==== public methods ====

      ;; this is called when vim mode is turned off
      (define/public (turn-off-vim-effects!)
        (unhighlight-ranges/key 'drracket-vim-highlight)
        (hide-caret #f))

      (define/public (on-initialization)
        (hide-caret #t)
        (do-caret-update))

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
               tabify-selection get-text
               scroll-to-position

               ;; from color:text<%>
               skip-whitespace

               ;; from text:basic<%>
               highlight-range unhighlight-ranges/key)

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

        ;; only show caret in insert mode
        (cond [(eq? new-mode 'insert)
               (hide-caret #f)]
              [else (hide-caret #t)])

        (when (eq? new-mode 'visual-line)
          (set! visual-line-mode-direction 'same)
          (set-position vim-position)
          (move-position 'left #f 'line)
          (move-position 'right #t 'line))
        (when (eq? new-mode 'visual)
          ;; extend selection when entering visual mode to avoid having
          ;; nothing selected initially
          (set-position vim-position)
          (move-position 'right #t))

        (when (eq? new-mode 'search)
          (set! search-queue (make-queue)))
        (when (and (eq? new-mode 'command)
                   (eq? old-mode 'insert))
          (set-vim-position! (get-start-position))
          (unless (at-start-of-line?)
            (cmd-move-position 'left)))

        (when (eq? new-mode 'insert)
          (set-position vim-position))

        (update-mode!)
        (do-caret-update))

      ;; called to set the vim position, needed to make sure GUI updates are done
      ;; after a position is set (since `after-set-position` is not called for this)
      (define (set-vim-position! pos [scroll? #t])
        (set! vim-position pos)
        (set! current-column-position
              (- vim-position
                 (line-start-position (position-line vim-position))))
        (when scroll?
          (scroll-to-position pos))
        (do-caret-update))

      ;; handle the GUI portion of setting the mode line
      (define/private (update-mode!)
        (send parent-frame set-vim-status-message (mode-string)))

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
           (define pos
             (if (eq? line 'last-line)
                 (line-start-position (last-line))
                 (line-start-position (sub1 line))))
           (set-vim-position! pos)]
          [_ (handle-simple-command command)])

        (unless (or (eq? command 'single-repeat)
                    (movement-command? command))
          (set! last-command command)))

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
           (set-position (skip-whitespace-forward (get-start-position)))]
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
          ['left          (cmd-move-position 'left)]
          ['down          (cmd-move-position 'down)]
          ['up            (cmd-move-position 'up)]
          ['right         (cmd-move-position 'right)]
          ['next-page     (cmd-move-position 'down #f 'page)]
          ['previous-page (cmd-move-position 'up #f 'page)]
          ['next-word     (cmd-move-position 'right #f 'word)
                          (set-vim-position! (skip-whitespace-forward))]
          ['previous-word (cmd-move-position 'left #f 'word)]
          ['continue
           (define-values (start end) (get-current-line-start-end))
           (cond [(and (or (= (sub1 end) vim-position) (empty-line?))
                       ;; only move if we're not about to hit the end or
                       ;; the next (and last) line is blank
                       (or (not (= (add1 vim-position) (last-position)))
                           (equal? #\newline
                                   (get-character vim-position))))
                  (cmd-move-position 'down)
                  (cmd-move-position 'left #f 'line)]
                 [else
                  (cmd-move-position 'right)])]
          ['start-of-line         (cmd-move-position 'left #f 'line)]
          ['end-of-line           (cmd-move-position 'right #f 'line)]
          ['start-of-line-content (as-edit-sequence
                                    (cmd-move-position 'left #f 'line)
                                    (set-vim-position! (skip-whitespace-forward)))]
          ['match (do-matching-paren
                    (λ (dir s e)
                      (match dir
                        ['backward (set-vim-position! s)]
                        ['forward  (set-vim-position! (sub1 e))])))]
          ['start-of-file (cmd-move-position 'home #f)]
          ['end-of-file   (cmd-move-position 'end #f)]

          ;; editing
          ['join-line            (do-join-line)]
          ['delete-at-cursor     (do-delete-insertion-point)]
          ['delete-before-cursor (do-delete-before-insertion-point)]
          ['toggle-case          (do-toggle-case)]

          ;; FIXME: in vim this can call out to an external program, but
          ;;        for now it only does the default behavior of indenting
          ['filter-line
           (define-values (start end) (get-current-line-start-end))
           (tabify-selection start end)]

          ;; copy & paste & editing
          ['delete-rest  (delete-until-end)]
          ['delete-line  (do-delete-line)]
          ['yank-line    (do-line (λ (s e) (send this copy #f 0 s e)))]
          ['paste        (do-paste)]
          ['paste-before (do-paste #f)]
          ['undo         (undo)]
          ['redo         (redo)]

          ;; search
          ['search      (set-mode! 'search)]
          ['next-search (do-next-search #t)]
          ['prev-search (do-previous-search)]
          ['search-cursor
           (define-values (start end)
             (values (box vim-position) (box vim-position)))
           (find-wordbreak start end 'selection)
           (set! search-string
                 (get-text (unbox start) (unbox end) #t))
           ;; we want the next hit, so move past this word first
           (move-position 'right #f 'word)
           (move-position 'right)
           (do-next-search)
           (set-mode! 'command)]

          ['single-repeat
           (when last-command
             (handle-command last-command))]

          [_   (void)]))

      (define/private (handle-motion-command command)
        (match-define (motion-command operation motion) command)
        (set-position vim-position)
        (match operation
          ['change (handle-motion motion
                                  (λ (s e) (send this kill 0 s e))
                                  void
                                  (λ ()
                                    (set-vim-position! (get-start-position))
                                    (set-mode! 'insert)))]
          ['delete (handle-motion motion
                                  (λ (s e) (send this kill 0 s e))
                                  void
                                  (λ ()
                                    (set-vim-position! (get-start-position))
                                    (adjust-caret-eol)))]
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
      (define/private (do-paste [after? #t])
        (cond [(or (eq? paste-type 'line)
                   (eq? paste-type 'line-end))
               (begin-edit-sequence)
               (define line (position-line vim-position))
               (cond [after?
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
                        (delete (line-start-position (+ line diff-lines))))]
                     [else
                      (paste 0 (line-start-position (sub1 line)))])
               (end-edit-sequence)]
              [after?
               (define old-pos vim-position)
               (define line (position-line old-pos))
               (define end (line-end-position line))
               (set-vim-position! (add1 old-pos))
               (cond [;; caret is as far right as it can go in command
                      (= (sub1 end) vim-position)
                      (begin-edit-sequence)
                      (insert " " end) ; dummy character, gets deleted
                      (set-vim-position! (add1 old-pos))
                      (paste 0 vim-position)
                      (delete (line-end-position line))
                      (adjust-caret-eol)
                      (end-edit-sequence)]
                     [else
                      (define old-last (last-position))
                      (paste 0 vim-position)
                      (define new-last (last-position))
                      ;; vim stays at the end of the paste, not right after
                      (set-vim-position! (+ vim-position (- new-last old-last 1)))])]
              [else
               (define old-last (last-position))
               (paste 0 vim-position)
               (define new-last (last-position))
               (set-vim-position! (+ vim-position (- new-last old-last 1)))]))

      ;; handle mark setting and navigation
      (define/private (handle-mark command)
        (match-define (mark-command kind mark) command)
        (match kind
          ['goto-mark-line
           (define mark-pos (lookup-mark mark))
           (when mark-pos
             (define mark-line (position-line mark-pos))
             (set-vim-position! (line-start-position mark-line)))]
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
                   (cmd-move-position 'left #f 'line))))

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
               (cmd-move-position 'left #f 'word)
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

      (define/private (do-delete-before-insertion-point)
        (unless (or (empty-line?) (at-start-of-line?))
          (as-edit-sequence
            (define pos (sub1 vim-position))
            (kill 0 pos (add1 pos))
            (cmd-move-position 'left)
            (adjust-caret-eol))))

      ;; like the move-position method in texts, but this method adjusts both
      ;; the vim position and text position for command/visual mode
      (define/private (cmd-move-position code [extend? #f] [kind 'simple])
        (begin-edit-sequence)
        (define-values (text-start text-end)
          (values (get-start-position) (get-end-position)))
        (define old-column-position current-column-position)

        ;; since we use text's move-position to figure out how to move, first line
        ;; up the vim/text positions and then do a move
        (set-position vim-position 'same)
        (move-position code extend? kind)
        (set-vim-position! (get-start-position) (not (eq? kind 'page)))

        ;; Don't allow navigation to the "end of line" position
        ;; since this would go "off the end" in vim
        (when (eq? mode 'command)
          (when (and (not (empty-line?))
                     (at-end-of-line?))
            (set-position (sub1 vim-position))
            (set-vim-position! (sub1 vim-position))))

        ;; implements vim's tracking of the column to move to
        (when (and (or (eq? code 'up) (eq? code 'down))
                   (eq? kind 'simple))
          (define target-pos
            (+ (line-start-position (position-line vim-position))
               old-column-position))
          (define end-pos
            (line-end-position (position-line vim-position)))
          (define up/down-pos
            (if (>= target-pos end-pos)
                (max vim-position (sub1 end-pos))
                target-pos))
          (set-position up/down-pos)
          (set-vim-position! up/down-pos)
          ;; we don't want to update this here since the line may have
          ;; ended before the recorded column
          (set! current-column-position old-column-position))

        ;; now handle how we reset the text position
        (cond [(and (= text-start text-end)
                    (not (eq? mode 'visual))
                    (not (eq? mode 'visual-line)))
               ;; if the selection was a single position to begin with, we update
               ;; position based on the current character
               (define char (get-character vim-position))
               (when (or (equal? char #\))
                         (equal? char #\])
                         (equal? char #\}))
                 (move-position 'right))]
              [else
               (set-position text-start text-end)])

        (do-caret-update)
        (end-edit-sequence))

      ;; move the position in visual mode, making sure to move the cursor
      ;; independently of the visual selection
      (define/private (vis-move-position code [kind 'simple])
        (begin-edit-sequence)
        (define old-position vim-position)

        ;; first move cursor, this shouldn't clobber the text positions
        (cmd-move-position code #f kind)

        (match* (mode code)
          [('visual 'down)
           (cond [(> vim-position (get-end-position))
                  (set-position (get-start-position) (add1 vim-position))]
                 [(> vim-position (get-start-position))
                  (set-position vim-position (get-end-position))])]
          [('visual 'up)
           (cond [(< vim-position (get-start-position))
                  (set-position vim-position (get-end-position))]
                 [(< vim-position (get-end-position))
                  (set-position (get-start-position) (add1 vim-position))])]
          [('visual 'left)
           (cond [(or (= (get-start-position) old-position)
                      (= (get-start-position) (sub1 (get-end-position))))
                  (move-position 'left #t)]
                 [else
                  (set-position (get-start-position)
                                (sub1 (get-end-position)))])]
          [('visual 'right)
           (cond [(or (>= vim-position (get-end-position))
                      (= (get-start-position) (sub1 (get-end-position))))
                  (move-position 'right #t)]
                 [else
                  (set-position (add1 (get-start-position))
                                (get-end-position))])]
          [('visual-line (or 'left 'right))
           (void)]
          [('visual-line 'down)
           (match visual-line-mode-direction
             [(or 'same 'down)
              (set-position (get-start-position)
                            (line-end-position (position-line vim-position)))
              (set! visual-line-mode-direction 'down)]
             ['up
              (set-position (line-start-position (position-line vim-position))
                            (get-end-position))
              (when (= (position-line (get-start-position))
                       (position-line (get-end-position)))
                (set! visual-line-mode-direction 'same))])]
          [('visual-line 'up)
           (match visual-line-mode-direction
             [(or 'same 'up)
              (set-position (line-start-position (position-line vim-position))
                            (get-end-position))
              (set! visual-line-mode-direction 'up)]
             ['down
              (set-position (get-start-position)
                            (line-end-position (position-line vim-position)))
              (when (= (position-line (get-start-position))
                       (position-line (get-end-position)))
                (set! visual-line-mode-direction 'same))])]
          [('visual-line 'prior)
           (when (equal? visual-line-mode-direction 'same)
             (set! visual-line-mode-direction 'up)
             (move-position 'right #f 'line)
             (move-position 'left #t 'line))
           (move-position 'up #t 'page)]
          [('visual-line 'next)
           (when (equal? visual-line-mode-direction 'same)
             (set! visual-line-mode-direction 'down)
             (move-position 'left #f 'line)
             (move-position 'right #t 'line))
           (move-position 'down #t 'page)])

        (do-caret-update)
        (end-edit-sequence))

      ;; ReplaceCommand -> Void
      ;; FIXME: make this work correctly for visual mode, etc.
      (define/private (handle-replace command)
        (match-define (replace-command char) command)
        (define pos vim-position)
        (define eol? (at-end-of-line? 1))
        (begin-edit-sequence)
        (do-delete-insertion-point)
        (insert char pos)
        (when eol?
          (cmd-move-position 'right))
        (end-edit-sequence))

      ;; (is-a?/c key-event%) -> void?
      (define/private (do-visual event)
        (cond
         [(check-escape event) (set-mode! 'command)]
         [else
          (match (send event get-key-code)
            ;; visual movement
            [#\b (vis-move-position 'left 'word)]
            [#\w (vis-move-position 'right 'word)]
            [#\$ (vis-move-position 'right 'line)]
            [#\^ (vis-move-position 'left 'line)]
            [(or #\h 'left)  (vis-move-position 'left)]
            [(or #\j 'down)  (vis-move-position 'down)]
            [(or #\k 'up)    (vis-move-position 'up)]
            [(or #\l 'right) (vis-move-position 'right)]

            ;; copy & paste
            [#\d (visual-kill)]
            [#\x (visual-kill)]
            [#\y (visual-copy)]
            [#\p (begin (paste)
                        (set-mode! 'command))]

            ;; toggling visual modes
            [#\v (if (eq? mode 'visual)
                     (set-mode! 'command)
                     (set-mode! 'visual))]
            [#\V (if (eq? mode 'visual-line)
                     (set-mode! 'command)
                     (set-mode! 'visual-line))]

            [#\tab (super on-local-char event)]
            [_ (void)])]))

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
                 (set-position vim-position)
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
                 (set-vim-position! (get-start-position))
                 (end-edit-sequence)]
                ;; start a fresh search
                [else
                 (set-searching-state search-string #f #t #f)
                 (finish-pending-search-work)
                 (when (get-replace-search-hit)
                   (set-vim-position! (get-replace-search-hit)))])))

      ;; [position] -> void
      ;; execute a search going backwards from start-pos
      (define/private (do-previous-search [start-pos vim-position])
        (when search-string
          (define bubbles (get-search-bubbles))
          ;; ASSUMPTION: bubbles are ordered by position
          (define matching-bubble
            (for/last ([bubble (in-list bubbles)]
                       #:when (< (caar bubble) start-pos))
              bubble))
          (cond [(null? bubbles) (void)]
                [matching-bubble
                 (set-vim-position! (caar matching-bubble))]
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
           (set-vim-position! (line-start-position (sub1 num)))]
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
      (define/private (do-join-line)
        (define newline-pos (find-newline 'forward vim-position))
        (when newline-pos
          (begin-edit-sequence)
          (delete newline-pos)
          (let loop ([char (get-character newline-pos)])
            (when (and (char-whitespace? char)
                       (not (eq? #\newline char)))
              (delete newline-pos)
              (loop (get-character newline-pos))))
          (cond [(and (> newline-pos 1)
                      (not (char-whitespace? (get-character (- newline-pos 2)))))
                 (insert #\space (sub1 newline-pos))
                 (set-vim-position! (sub1 newline-pos))]
                [(> newline-pos 1)
                 (set-vim-position! (- newline-pos 2))])
          (end-edit-sequence)))

      (define/private (skip-whitespace-forward [pos #f])
        (skip-whitespace (or pos vim-position)
                         'forward
                         #f))

      (define/private (skip-whitespace-backward [pos #f])
        (skip-whitespace (or pos vim-position)
                         'backward
                         #f))

      ;; toggle case of current character, then move
      (define/private (do-toggle-case)
        (define ch (get-character vim-position))
        (define new-ch
          (if (char-upper-case? ch)
              (char-downcase ch)
              (char-upcase ch)))
        (delete (add1 vim-position))
        (insert new-ch vim-position)
        (cmd-move-position 'right))

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
        (set-vim-position! (get-start-position))
        (set-position vim-position)
        (set-mode! 'command))

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
          (cmd-move-position 'left)))

      (super-new))))

