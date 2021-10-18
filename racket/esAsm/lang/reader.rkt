#lang racket

(provide (rename-out [read-syntax-esAsm read-syntax]))


(require "../parser.rkt")

(define (read-esAsm in)
  (read-syntax-esAsm #f in))

(define (read-syntax-esAsm src in)
  (define module-datum `(module esAsm-syntax esAsm/esAsm
                          ,(parse-esAsm in)))
  (datum->syntax #f module-datum))