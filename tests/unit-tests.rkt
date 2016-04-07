#lang at-exp racket/gui

;; Tests for DrRacket vim plugin

(require "utils.rkt"
         racket/format
         rackunit)

(define ctrl-c
  (new key-event% [key-code #\c] [control-down #t]))

(define ctrl-lsb
  (new key-event% [key-code #\[] [control-down #t]))

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
 '(down #\x)
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
 `(#\l #\i #\a #\b ,ctrl-c #\x)
 @~a{#alang racket
     abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 `(#\l #\i #\a #\b ,ctrl-lsb #\x)
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
 '(#\space 'up #\x)
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
  @~a{abc def}
  '(#\w #\x)
  @~a{abc ef})

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
 @~a{#lang racket
     foobar
     barbaz
     bazqux}
 '(#\2 #\G #\o #\a)
 @~a{#lang racket
     foobar
     a
     barbaz
     bazqux})

(check-vim
 @~a{(    )}
 '(#\% #\x)
 @~a{(    })

(check-vim
 @~a{(    ) a}
 '(#\% #\x)
 @~a{(     a})

;; issue #62
(check-vim
 @~a{(     a}
 '(#\% #\x)
 @~a{     a})

(check-vim
 @~a{)     a}
 '(#\% #\x)
 @~a{     a})

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

;;; replace

(check-vim
 @~a{#lang racket}
 '(#\r #\a)
 @~a{alang racket})

(check-vim
 @~a{#lang racket}
 `(#\r shift #\A)
 @~a{Alang racket})

(check-vim
 @~a{#lang racket}
 `(#\r shift #\:)
 @~a{:lang racket})

;; issue #58 for replace at end of line
(check-vim
 @~a{#lang racket}
 '(#\$ #\r #\a #\x)
 @~a{#lang racke})

;;; change
(check-vim
 @~a{#lang racket}
 '(#\c #\c #\x)
 @~a{x})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\c #\j #\x)
 @~a{x})

(check-vim
 @~a{abcdef}
 '(#\c #\j #\x)
 @~a{bcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\l #\l #\c #\h #\x)
 @~a{#xang racket
     abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\j #\c #\k #\x)
 @~a{x})

(check-vim
 @~a{#lang racket}
 '(#\c #\k #\x)
 @~a{lang racket})

(check-vim
 @~a{#lang racket}
 '(#\c #\l #\x)
 @~a{xlang racket})

(check-vim
 @~a{#lang racket}
 '(#\S #\x)
 @~a{x})

(check-vim
 @~a{#lang racket}
 '(#\l #\l #\C #\x)
 @~a{#lx})

(check-vim
 @~a{abcdef}
 '(#\c #\a #\w #\x)
 @~a{x})

(check-vim
 @~a{abc def}
 '(#\l #\l #\l #\l #\c #\a #\w #\x)
 @~a{abcx})

(check-vim
 @~a{abc def ghi}
 '(#\l #\l #\l #\l #\c #\a #\w #\x)
 @~a{abcx ghi})

(check-vim
 @~a{(abc def)}
 '(#\c #\% #\x)
 @~a{x})

;;; deletion / yanking / pasting
(check-vim
 @~a{#lang racket
     abcdef}
 '(#\d #\d)
 @~a{abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\d #\j)
 @~a{})

(check-vim
 @~a{abcdef}
 '(#\d #\j)
 @~a{abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\l #\l #\d #\h)
 @~a{#ang racket
     abcdef})

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\j #\d #\k)
 @~a{})

(check-vim
 @~a{#lang racket}
 '(#\d #\k)
 @~a{#lang racket})

(check-vim
 @~a{abc def}
 '(#\l #\l #\l #\l #\d #\a #\w)
 @~a{abc})

(check-vim
 @~a{abc def}
 '(#\l #\l #\l #\l #\d #\a #\w #\i #\d)
 @~a{abdc})

(check-vim
 @~a{abc def ghi}
 '(#\l #\l #\l #\l #\d #\a #\w)
 @~a{abc ghi})

(check-vim
 @~a{abc def ghi}
 '(#\d #\a #\w)
 @~a{def ghi})

(check-vim
 @~a{abc
     def}
 '(#\d #\a #\w)
 "\ndef")

(check-vim
 @~a{abc def}
 '(#\d #\w)
 @~a{def})

(check-vim
 @~a{abc def}
 '(#\d #\b)
 @~a{abc def})

(check-vim
 @~a{abc def}
 '(#\w #\d #\b)
 @~a{def})

(check-vim
 @~a{abc def ghi}
 '(#\w #\d #\w)
 @~a{abc ghi})

(check-vim
 @~a{abc def ghi}
 '(#\l #\d #\w)
 @~a{adef ghi})

(check-vim
 @~a{#lang racket}
 '(#\d escape #\d)
 @~a{#lang racket})

(check-vim
 @~a{#lang racket}
 `(#\d ,ctrl-c #\d)
 @~a{#lang racket})

(check-vim
 @~a{#lang racket}
 `(#\d ,ctrl-lsb #\d)
 @~a{#lang racket})

(check-vim
 @~a{#lang racket}
 '(#\v #\l #\l #\l #\y #\p)
 @~a{##lanlang racket})

(check-vim
 @~a{#lang racket
     123
     456}
 '(#\V #\y #\p)
 @~a{#lang racket
     #lang racket
     123
     456})

(check-vim
 @~a{racket}
 '(#\y #\a #\w #\p #\x)
 @~a{rrackeacket})

(check-vim
 @~a{racket}
 '(#\y #\a #\w #\$ #\p #\x)
 @~a{racketracke})

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

(check-vim
 @~a{#lang racket
     abcdef}
 '(#\y #\space #\j #\p)
 @~a{#lang racket
     a#bcdef})

(check-vim
 @~a{#lang racket}
 '(#\d #\space)
 @~a{lang racket})

(check-vim
 @~a{#lang racket}
 '(#\d #\l)
 @~a{lang racket})

;; Issue #66
(check-vim
 @~a{foo
     bar}
 '(#\v #\l #\y #\j #\h #\h #\p #\p #\p)
 @~a{foo
     bfofofoar})

;;; ex commands
(check-vim
 @~a{#lang racket
     abcdef
     lastline}
 '(#\: #\3 #\return #\x)
 @~a{#lang racket
     abcdef
     astline})

(check-vim
 @~a{#lang racket
     abcdef
     lastline}
 '(#\j #\: #\0 #\return #\x)
 @~a{#lang racket
     bcdef
     lastline})

(check-vim
 @~a{#lang racket
     abcdef
     lastline}
 '(#\j #\: #\1 #\1 #\3 #\return #\x)
 @~a{#lang racket
     abcdef
     astline})

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

(check-vim
 @~a{test

     test}
 '(#\/ #\t #\e #\s #\t #\return #\n #\x)
 @~a{test

     est})

(check-vim
 @~a{test

     test}
 '(#\/ #\t #\e #\s #\t #\return #\n #\n #\x)
 @~a{est

     test})

(check-vim
 @~a{test

     test}
 '(#\l #\/ #\t #\e #\s #\t #\return #\N #\x)
 @~a{est

     test})

(check-vim
 @~a{test

     test}
 '(#\/ #\t #\e #\s #\t #\return #\n #\N #\x)
 @~a{est

     test})

(check-vim
 @~a{test

     test

     test}
 '(#\/ #\t #\e #\s #\t #\return #\n #\n #\N #\x)
 @~a{test

     est

     test})

(check-vim
 @~a{test

     test}
 '(#\l #\/ #\t #\e #\s #\t #\return #\N #\N #\x)
 @~a{test

     est})

;;; Select / visual mode
(check-vim
 @~a{#lang racket
     (+ 1 2)}
 '(#\V #\j #\k #\x)
 @~a{(+ 1 2)})

(check-vim
 @~a{abcdef
     ghijkl}
 '(#\V #\j #\y #\j #\p)
 @~a{abcdef
     ghijkl
     abcdef
     ghijkl})

(check-vim
 @~a{abcdef
     ghijkl
     mnopqr}
 '(#\V #\j #\y #\j #\p)
 @~a{abcdef
     ghijkl
     abcdef
     ghijkl
     mnopqr})

(check-vim
 @~a{abcdef
     ghijkl
     mnopqr}
 '(#\V #\j #\j #\y #\j #\j #\p)
 @~a{abcdef
     ghijkl
     mnopqr
     abcdef
     ghijkl
     mnopqr})

;; issue #43
(check-vim
 @~a{      abcdef}
 '(#\A escape #\I #\z)
 @~a{      zabcdef})

(check-vim
 @~a{      abcdef}
 `(#\A ,ctrl-c #\I #\z)
 @~a{      zabcdef})

(check-vim
 @~a{      abcdef}
 `(#\A ,ctrl-lsb #\I #\z)
 @~a{      zabcdef})

;; issue #45
(check-vim
 @~a{abcdef}
 '(#\x #\p)
 @~a{bacdef})

;; issue #51
(check-vim
 @~a{abc
     abc}
 '(#\J #\x)
 @~a{abc bc})

(check-vim
 @~a{abc

     abc}
 '(#\J #\x)
 @~a{abc
     abc})

;; Check 'o', issue #47
(check-vim
 @~a{a
     b}
 '(#\o #\c)
 @~a{a
     c
     b})

;; Issue #63, o indentation
(check-vim
 @~a{(let ([x 5])
       x)}
 '(#\o #\x)
 @~a{(let ([x 5])
       x
       x)})

(check-vim
 @~a{(let ([x 5])
       x)}
 '(#\j #\O #\x)
 @~a{(let ([x 5])
       x
       x)})

;; Repeate command

(check-vim
 @~a{a
     b
     c
     d
     e
     f}
 '(#\g #\g #\3 #\j #\o #\b)
 @~a{a
     b
     c
     d
     b
     e
     f})

;; Issue #34, #\space on empty line
(check-vim
 "\na\n"
 '(#\space #\x)
 "\n\n")

;; Related to issue #34, space near end of file
(check-vim
 "\nab"
 '(#\space #\space #\space #\x)
 "\na")

(check-vim
 "\n"
 '(#\space #\i #\a)
 "\na")

;; Issue #25
(check-vim
 "\n\n"
 '(#\a #\b)
 "b\n\n")

;; Issue #65
(check-vim
 @~a{#lang racket}
 '(#\s #\t)
 @~a{tlang racket})

;; Issue #64
(check-vim
 @~a{(let ([x 5])
     x)}
 '(#\j #\= #\=)
 @~a{(let ([x 5])
       x)})

;; Issue #67
(check-vim
 @~a{#lang racket}
 '(#\$ #\0 #\x)
 @~a{lang racket})

;; Issue #39
(check-vim
  @~a{#lang racket

      (define x
        (compile
         #'(begin
             +
             (define + 5)
             +)))}
  '(#\: #\3 #\return #\V #\j #\j #\j #\j #\j #\y
    #\: #\9 #\return #\p)
  @~a{#lang racket

      (define x
        (compile
         #'(begin
             +
             (define + 5)
             +)))
      (define x
        (compile
         #'(begin
             +
             (define + 5)
             +)))})

(exit)
