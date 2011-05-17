#lang racket/gui

(require drracket/tool
         framework
         "private/text.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  
  (define vim-frame-mixin
    (mixin (drracket:unit:frame<%>) ()
  
      (inherit get-definitions-text)
  
      (define/override (edit-menu:between-find-and-preferences edit-menu)
        (super edit-menu:between-find-and-preferences edit-menu)
  
        (new checkable-menu-item%
             [label "Vim Mode"]
             [parent edit-menu]
             [callback 
               (lambda (i e) (send (get-definitions-text) toggle-vim!))]
             [checked (send (get-definitions-text) vim?)]))
  
      (super-new)))

  (define (phase1) (void))
  (define (phase2) (void))

  (preferences:set-default 'drracket:vim-emulation? #f (lambda (x) (boolean? x)))
  (drracket:get/extend:extend-definitions-text vim-emulation-mixin)
  (drracket:get/extend:extend-unit-frame vim-frame-mixin))
