#lang racket

(provide (rename-out [read-syntax-moji read-syntax]))


(require "../parser.rkt")

(define (read-moji in)
  (read-syntax-moji #f in))

(define (read-syntax-moji src in)
  (define module-datum `(module moji-syntax esAsm/esAsm
                          ,(parse-moji in)))
  (datum->syntax #f module-datum))