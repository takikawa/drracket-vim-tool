#lang racket/base

;; Parse and output representation of vim commands

(require racket/class
         racket/function
         racket/gui/base
         racket/match)

(provide parse-command
         (struct-out motion-command)
         (struct-out mark-command)
         (struct-out repeat-command)
         (struct-out repeat-motion)
         (struct-out replace-command)
         (struct-out goto-command)
         (struct-out find-char-command)
         movement-command?)

;; A Command is one of
;;   - Symbol
;;   - (repeat-command Repeat Command)
;;   - (motion-command Operator Motion)
;;   - (mark-command Mark-Kind Char)
;;   - (replace-command Char)
;;   - (goto-command (U 'line Integer))
;;   - (find-char-command Direction Inclusive? Char)
(struct repeat-command (repeat command))
(struct motion-command (operator motion))
(struct mark-command (kind mark))
(struct replace-command (char))
(struct goto-command (line))
(struct find-char-command (direction inclusive? char))

;; A Repeat is an integer
;; An Operator (for a motion command) is one of
;;   (or/c 'change
;;         'delete
;;         'yank
;;         'swap-case
;;         'lowercase
;;         'uppercase
;;         'format
;;         'rot13
;;         'shift-right
;;         'shift-left
;;         'filter)
;;
;; TODO: Operator leaves out
;;       folding, and function calls from vim
;;
;; A Motion is one of
;;   - 'a-word
;;   - 'inner-word
;;   - 'word-forward
;;   - 'word-backward
;;   - 'sentence
;;   - 'a-paragraph
;;   - 'match
;;   - 'double-quote
;;   - (repeat-motion Repeat Motion)
;;     where Motion is a symbol
(struct repeat-motion (repeat motion))

;; A Direction (for find-char-command) is one of 'forward or 'backward
;; An Inclusive? is a boolean:
;;   - #t for f/F commands (cursor lands on the character)
;;   - #f for t/T commands (cursor lands before/after the character)

;; These codes are ignored in the sense that they are skipped over
;; and they don't interrupt a command sequence.
(define ignored-codes
  (list 'shift
        'rshift
        'control
        'rcontrol
        'scroll
        'wheel-up
        'wheel-down
        'wheel-left
        'wheel-right
        'release
        'press))

;; Key-Code -> Boolean
(define (digit? char)
  (and (char? char)
       (memq char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

;; Key-Event% (-> Key-Event%) -> Command
;; Parse a single command
(define (parse-command key *next-key)

  ;; helper that ignores ignored characters
  (define (next-key)
    (let loop ()
      (define key (*next-key))
      (define code (send key get-key-code))
      (if (memq code ignored-codes)
          (loop)
          key)))

  (define code (send key get-key-code))
  (match code
    ;; repeats
    [(? digit? digit)
     ;; '0' cannot start a repeat sequence in vim
     #:when (not (eq? #\0 digit))
     (parse-repeat code next-key)]

    ;; multi-char/motion commands
    [#\d (parse-delete next-key)]
    [#\y (parse-yank next-key)]
    [#\m (parse-mark next-key 'save-mark)]
    [#\' (parse-mark next-key 'apostrophe)]
    [#\` (parse-mark next-key 'backtick)]
    [#\g (parse-global next-key)]
    [#\r #:when (not (send key get-control-down))
     (parse-replace next-key)]
    [#\= (parse-filter next-key)]
    [#\c (parse-change next-key)]
    [#\w #:when (send key get-control-down)
     (parse-window next-key)]
    [#\> (parse-shift-right next-key)]
    [#\< (parse-shift-left next-key)]
    [#\f #:when (not (send key get-control-down))
     (parse-find-char next-key 'forward #t)]
    [#\F (parse-find-char next-key 'backward #t)]
    [#\t (parse-find-char next-key 'forward #f)]
    [#\T (parse-find-char next-key 'backward #f)]

    ;; insertion / change
    [#\a 'insert-end]
    [#\A 'insert-end-line]
    [#\i 'insert]
    [#\I 'insert-line]
    [#\O 'insert-previous-line]
    [#\o 'insert-next-line]
    [#\s 'insert-at-delete]
    [#\S 'change-line]
    [#\C 'change-rest]

    ;; modes
    [#\v 'visual]
    [#\V 'visual-line]
    [#\: 'ex]

    ;; movement
    [(or #\h 'left)  'left]
    [(or #\j 'down)  'down]
    [(or #\k 'up)    'up]
    [(or #\l 'right) 'right]
    [#\f #:when (send key get-control-down)
     'next-page]
    [#\b #:when (send key get-control-down)
     'previous-page]
    [#\w     'next-word]
    [#\b     'previous-word]
    ['prior  'previous-page]
    ['next   'next-page]
    [#\space 'continue]
    [#\0     'start-of-line]
    [#\$     'end-of-line]
    [#\^     'start-of-line-content]
    [#\%     'match]
    [#\G     'end-of-file]

    ;; editing
    [#\J     'join-line]
    [#\x     'delete-at-cursor]
    [#\X     'delete-before-cursor]
    [#\~     'toggle-case]

    ;; copy & paste & editing
    [#\D     'delete-rest]
    [#\p     'paste]
    [#\P     'paste-before]
    [#\u     'undo]
    [#\r #:when (send key get-control-down)
     'redo]

    ;; search
    [#\/     'search]
    [#\n     'next-search]
    [#\N     'prev-search]
    [#\*     'search-cursor]

    ;; other
    [#\.     'single-repeat]
    [#\;     'repeat-find-char]
    [#\,     'repeat-find-char-opposite]

    [_ #f]))

(define (parse-repeat digit next-key)
  (define (char-numeric->number x)
    (- (char->integer x) (char->integer #\0)))
  (let loop ([num (char-numeric->number digit)])
    (define event (next-key))
    (match (send event get-key-code)
      [#\G
       (goto-command (if (zero? num) 'last-line num))]
      [#\g
       (match (send (next-key) get-key-code)
         [#\g (goto-command num)]
         [_   #f])]
      [(? digit? digit)
       (loop (+ (char-numeric->number digit) (* 10 num)))]
      [_
       (repeat-command num (parse-command event next-key))])))

(define (parse-replace next-key)
  (define char (send (next-key) get-key-code))
  (and (char? char)
       (replace-command char)))

(define (parse-delete next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\d 'delete-line]
    [c
     (define motion (parse-motion key next-key))
     (and motion (motion-command 'delete motion))]))

(define (parse-yank next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\y 'yank-line]
    [c
     (define motion (parse-motion key next-key))
     (and motion (motion-command 'yank motion))]))

(define (parse-global next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\g 'start-of-file]
    [#\t 'next-tab]
    [#\T 'prev-tab]
    [_   #f]))

(define (parse-mark next-key kind)
  (define key (next-key))
  (define code (send key get-key-code))
  (and (mark-char? code)
       (match kind
         ['apostrophe (mark-command 'goto-mark-line code)]
         ['backtick   (mark-command 'goto-mark-char code)]
         ['save-mark  (mark-command 'save-mark      code)])))

(define (mark-char? key)
  (and (char? key)
       (char>=? key #\a)
       (char<=? key #\z)))

(define (parse-filter next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\= 'filter-line]
    [_   #f]))

(define (parse-change next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\c 'change-line]
    ;; FIXME: implement change with motions
    [_
     (define motion (parse-motion key next-key))
     (and motion (motion-command 'change motion))]))

;; window commands with ctrl-w
(define (parse-window next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  ;; ctrl can be down or up for most of these
  (match code
    [#\w 'window-next]
    [_   #f]))

(define (parse-shift-right next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\> 'shift-right]
    ;; FIXME: support motions
    [_   #f]))

(define (parse-shift-left next-key)
  (define key (next-key))
  (define code (send key get-key-code))
  (match code
    [#\< 'shift-left]
    ;; FIXME: support motions
    [_   #f]))

(define (parse-find-char next-key direction inclusive?)
  (define key (next-key))
  (define char (send key get-key-code))
  (and (char? char)
       (not (check-escape-char char))
       (find-char-command direction inclusive? char)))

;; check if a character is escape-like (should not be used in find commands)
(define (check-escape-char char)
  (or (eq? char 'escape)
      (char=? char #\return)
      (char=? char #\newline)
      (char=? char #\tab)))

(define (parse-motion first-key next-key)
  (define code (send first-key get-key-code))
  (match code
    [#\a (match (send (next-key) get-key-code)
           [#\w 'a-word]
           [#\p 'a-paragraph]
           [(or #\b #\( #\)) 'a-block]
           [_    #f])]
    [#\i (match (send (next-key) get-key-code)
           [(or #\b #\( #\)) 'inner-block]
           [_ #f])]
    [#\h     'left]
    [#\j     'down]
    [#\k     'up]
    [#\w     'word-forward]
    [#\b     'word-backward]
    [#\%     'match]
    [#\"     'double-quote]
    [#\f     (parse-find-char next-key 'forward #t)]
    [#\F     (parse-find-char next-key 'backward #t)]
    [#\t     (parse-find-char next-key 'forward #f)]
    [#\T     (parse-find-char next-key 'backward #f)]
    [(or #\space #\l) 'right]
    [_   #f]))

;; check if the given command is a movement command
(define (movement-command? command)
  (or (goto-command? command)
      (find-char-command? command)
      (memq command
            '(left down up right
              next-page previous-page
              next-word previous-word
              continue
              start-of-line end-of-line
              start-of-line-content
              match
              start-of-file end-of-file
              repeat-find-char repeat-find-char-opposite))))
