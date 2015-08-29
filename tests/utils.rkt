#lang racket/base

;; Test harness for drracket vim mixin

(require "../private/text.rkt"
         framework
         racket/class
         racket/gui/base
         rackunit
         (for-syntax racket/base))

(provide check-vim)

;; A mock frame% that pretends to support vim features
(define mock-frame%
  (class frame%
    (super-new)
    (define/public (vim?) #t)
    (define/public (set-vim-status-message arg) (void))))

;; Need these only so editing actually works in the text%
(define frame (new mock-frame% [label "testing"]))
(define e-c (new editor-canvas% [parent frame]))

;; A mock text% class that pretends to support DrRacket features
(define mock-text%
  (class text:searching%
    (super-new)
    (define/public (forward-sexp) (void))
    (define/public (backward-sexp) (void))))

;; string (listof (U char symbol key-event)) -> (is-a?/c text%)
(define (do-text-actions initial-text keys)
  (define edit (new (vim-emulation-mixin mock-text%)))
  (set-field! parent-frame edit frame)
  (send e-c set-editor edit)
  (send edit insert initial-text 0)
  (send edit set-position 0)
  (send edit set-max-undo-history 'forever)
  (for ([key (in-list keys)])
    (define ke
      (if (is-a? key key-event%)
          key
          (new key-event% [key-code key])))
    (send edit on-local-char ke))
  (send edit get-text))

(define-syntax (check-vim stx)
  (syntax-case stx ()
    [(_ initial-text keys final-text)
     #`(let ([output (do-text-actions initial-text keys)])
         #,(syntax/loc stx (check-equal? output final-text)))]))
