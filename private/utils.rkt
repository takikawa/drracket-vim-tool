#lang racket/base

;; Utilities for DrRacket vim tool

(require racket/class)

(provide as-edit-sequence)

(define-syntax-rule (as-edit-sequence e ...)
  (begin
    (send this begin-edit-sequence)
    e ...
    (send this end-edit-sequence)))