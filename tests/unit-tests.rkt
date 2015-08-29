#lang at-exp racket/gui

;; Tests for DrRacket vim plugin

(require "utils.rkt"
         racket/format
         rackunit)

;;; basic movement and editing
(check-vim
 @~a{#lang racket
     abcdef}
 '(#\j #\x)
 @~a{#lang racket
     bcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\l #\i #\a #\b escape #\x)
 @~a{#alang racket
     abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\j #\A #\return #\( #\))
 @~a{#lang racket
     abcdef
     ()})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\space #\x #\space #\x #\space #\D)
 @~a{#ag
     abcdef})

(check-vim
 @~a{#lang
     abcdef}
 '(#\space #\space #\space #\space #\space #\x)
 @~a{#lang
     bcdef})

(check-vim
 @~a{#lang racket}
 '(#\space #\k #\x)
 @~a{#ang racket})

(check-vim
 @~a{#lang racket}
 '(#\space #\j #\x)
 @~a{#ang racket})

(check-vim
 @~a{#lang racket

     x}
 '(#\j #\h #\x)
 @~a{#lang racket

     x})

(check-vim
 @~a{x

     #lang racket}
 '(#\j #\l #\x)
 @~a{x

     #lang racket})

(check-vim
 @~a{#lang racket}
 '(#\a #\a #\b #\c)
 @~a{#abclang racket})

(check-vim
 @~a{}
 '(#\a #\a #\b #\c)
 @~a{abc})

(check-vim
 @~a{#lang racket
     foobar}
 '(#\G #\x)
 @~a{#lang racket
     fooba})

(check-vim
 @~a{(    )}
 '(#\% #\x)
 @~a{(    })

(check-vim
 @~a{(    ) a}
 '(#\% #\x)
 @~a{(     a})

(check-vim
 @~a{(    )}
 '(#\l #\% #\x)
 @~a{(   )})

(check-vim
 @~a{(    )}
 '(#\l #\l #\l #\l #\l #\% #\x)
 @~a{    )})

(check-vim
 @~a{(    )a}
 '(#\d #\%)
 @~a{a})

(check-vim
 @~a{(    )a}
 '(#\l #\l #\l #\l #\l #\d #\%)
 @~a{a})

(check-vim
 @~a{(    )a}
 '(#\y #\% #\% #\p)
 @~a{(    )(    )a})

;;; deletion / yanking / pasting
(check-vim
 @~a{#lang racket
     abcdef}
 '(#\d #\d)
 @~a{abcdef})

(check-vim
 @~a{#lang racket}
 '(#\v #\l #\l #\l #\y #\p)
 @~a{##lanlang racket})

;; next two tests for issue #20
(check-vim
 @~a{#lang racket}
 '(#\d #\d)
 @~a{})

(check-vim
 @~a{}
 '(#\d #\d)
 @~a{})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\j #\d #\d)
 @~a{#lang racket})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\d #\d #\j #\p)
 @~a{abcdef
     #lang racket})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\y #\y #\j #\p)
 @~a{#lang racket
     abcdef
     #lang racket})

;;; undo/redo
(check-vim
 @~a{#lang racket
     abcdef}
 '(#\d #\d #\u)
 @~a{#lang racket
     abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 `(#\d #\d #\u ,(new key-event% [key-code #\r] [control-down #t]))
 @~a{abcdef})

;;; searching
(check-vim
 @~a{#lang racket
     banana}
 '(#\/ #\b #\a #\n #\a #\n #\a #\return #\x)
 @~a{#lang racket
     anana})

(check-vim
 @~a{#lang racket
     banana}
 '(#\/ #\return #\x)
 @~a{lang racket
     banana})

(exit)
