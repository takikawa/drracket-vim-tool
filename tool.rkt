#lang racket/gui

(require drracket/tool
         "private/text.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)

  (define (phase1) (void))
  (define (phase2) (void))

  (drracket:get/extend:extend-definitions-text vim-emulation-mixin))
