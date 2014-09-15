#lang setup/infotab

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "Vim Emulation"))
(define drracket-tool-icons '(#f))

(define blurb '("Vim-style editing for DrRacket"))
(define categories '(devtools))
(define primary-file "tool.rkt")

(define deps '("base" "gui-lib" "data-lib" "drracket-plugin-lib"
               "unstable-list-lib"))
(define single-collection "drracket-vim-tool")

