#lang at-exp racket/base

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

;;; line deletion / yanking / pasting
(check-vim
 @~a{#lang racket
     abcdef}
 '(#\d #\d)
 @~a{abcdef})

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

(exit)
