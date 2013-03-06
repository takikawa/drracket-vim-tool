#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui
         "private/text.rkt")

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
             (λ (i e) (send (get-definitions-text) toggle-vim!))]
           [checked (send (get-definitions-text) vim?)]))

    ;; for the vim status (mode text, etc.)
    ;; TODO: this pattern is really horrible
    (define vim-status-parent-panel 'uninitialized)
    (define vim-status-panel 'uninitialized)
    (define vim-status-message 'uninitialized)

    ;; overriden to add a status panel
    (define/override (make-root-area-container cls parent)
      (set! vim-status-parent-panel
            (super make-root-area-container vertical-panel% parent))
      (define root (new cls [parent vim-status-parent-panel]))
      (set! vim-status-panel
            (new horizontal-panel%
                 [style '(border)]
                 [stretchable-height #f]
                 [parent vim-status-parent-panel]))
      (set! vim-status-message
            (new message%
                 [parent vim-status-panel]
                 [auto-resize #t]
                 [label ""]))
      root)

    (define/public (set-vim-status-message str)
      (send vim-status-message set-label str))

    (super-new)))

(define (phase1) (void))
(define (phase2) (void))

(preferences:set-default 'drracket:vim-emulation? #f boolean?)
(drracket:get/extend:extend-definitions-text vim-emulation-mixin)
(drracket:get/extend:extend-unit-frame vim-frame-mixin)
