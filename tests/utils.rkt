#lang racket/base

;; Test harness for drracket vim mixin

(require "../private/text.rkt"
         racket/class
         racket/gui/base
         rackunit)

(provide check-vim)

;; A mock frame% that pretends to support vim features
(define mock-frame%
  (class frame%
    (super-new)
    (define/public (set-vim-status-message arg) (void))))

;; Need these only so editing actually works in the text%
(define frame (new mock-frame% [label "testing"]))
(define e-c (new editor-canvas% [parent frame]))

;; A mock text% class that pretends to support DrRacket features
(define mock-text%
  (class text%
    (super-new)
    (define/public (get-tab)
      (new (class object%
             (super-new)
             (define/public (get-frame) frame))))
    (define/public (forward-sexp) (void))
    (define/public (backward-sexp) (void))
    (define/public (set-searching-state) (void))
    (define/public (get-search-hit-count) (void))
    (define/public (get-replace-search-hit) (void))
    (define/public (search-updates-pending?) (void))))

;; string (listof char) -> (is-a?/c text%)
(define (do-text-actions initial-text keys)
  (define edit (new (vim-emulation-mixin mock-text%)
                    [override-vim-emulation-preference? #t]))
  (send e-c set-editor edit)
  (send edit insert initial-text 0)
  (send edit set-position 0)
  (for ([key (in-list keys)])
    (send edit on-local-char (new key-event% [key-code key])))
  (send edit get-text))

(define-syntax-rule (check-vim initial-text keys final-text)
  (let ([output (do-text-actions initial-text keys)])
    (check-equal? output final-text)))
