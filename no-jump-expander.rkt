#lang racket

(require (for-syntax syntax/parse))

(provide #%top #%app #%datum #%top-interaction
         (rename-out [esAsm-mod-begin #%module-begin])
         (all-defined-out))


(define-syntax (esAsm-mod-begin stx)
  (syntax-parse stx
    [(esAsm-mod-begin (program LINE ...))
     #'(#%module-begin
        LINE ...)]))

(define-syntax (inst stx)
  (syntax-parse stx
    [(_ type args ...)
     #'(type args ...)]))

;
; global data
;
(define registers (make-hash))

;
; helpers for instructions
;
(define (reg r)
  (hash-ref registers r))

(define (num n) n)

(define (reg-set! r v)
  (hash-set! registers r v))

;
; instructions
;
(define (SHN val)
  (displayln val))

(define (SHA val)
  (displayln (integer->char val)))

(define (INP reg)
  (reg-set! reg (read)))

(define (MOV reg val)
  (reg-set! reg val))

(define (ADD reg v1 v2)
  (reg-set! reg (+ v1 v2)))

(define (SUB reg v1 v2)
  (reg-set! reg (- v1 v2)))
