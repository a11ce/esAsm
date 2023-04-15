#lang racket

(require "lexer.rkt"
         syntax/strip-context)

(provide read-syntax)

(define (read-syntax path port)
  (define ast
    (parse-esAsm port))
  (strip-context
   #`(module esAsm-mod esAsm/no-jump-expander
       #,ast)))

(module+ reader
  (provide read-syntax))