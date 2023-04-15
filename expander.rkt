#lang racket

(require (for-syntax syntax/parse))
         

(provide #%top #%app #%datum #%top-interaction
         (rename-out [esAsm-mod-begin #%module-begin])
         (all-defined-out))


;
; parse to racket
;
(define-syntax (inst stx)
  (syntax-parse stx
    [(_ type args ...)
     #'(λ () (type args ...))]))

(define-syntax (lab-decl stx)
  (syntax-parse stx
    [(_ lab)
     #'lab]))

;
; global data 
;
(define registers (make-hash))
(define on-fire #f)

(struct jump-signal (val))
(struct halt-signal ())

;
; helpers for instructions
;
(define (reg r)
  (hash-ref registers r))

(define (num n) n)

(define (reg-set! r v)
  (hash-set! registers r v))

(define (jump-to lab)
  (raise (jump-signal lab)))
              

;
; instructions
;
(define (SHN val)
  (displayln val))

(define (SHA val)
  (displayln (integer->char val)))

(define (MOV reg val)
  (reg-set! reg val))

(define (ADD reg v1 v2)
  (reg-set! reg (+ v1 v2)))

(define (SUB reg v1 v2)
  (reg-set! reg (- v1 v2)))

(define (JMP lab)
  (jump-to lab))

(define (JLT lab v1 v2)
  (when (v1 . < . v2)
    (jump-to lab)))

(define (JGT lab v1 v2)
  (when (v1 . > . v2)
    (jump-to lab)))

(define (JET lab v1 v2)
  (when (= v1 v2)
    (jump-to lab)))

(define (INP reg)
  (reg-set! reg (read)))

(define (HLT)
  (raise (halt-signal)))

(define (MFICOFSR reg)
  (if on-fire
      (reg-set! reg 1)
      (reg-set! reg 0)))

(define (run lines)
  (define jumping-to #f)
  (with-handlers
      ([halt-signal? void])
    (for ([line (in-cycle lines)])
      (with-handlers
          ([jump-signal?
            (λ (js)
              (set! jumping-to
                    (jump-signal-val js)))])
        (if (string? line)
            (when (equal? jumping-to line)
              (set! jumping-to #f))
            (unless jumping-to
              (line)))))))

(define-syntax (esAsm-mod-begin stx)
  (syntax-parse stx
    [(esAsm-mod-begin (program LINE ...))
     #'(#%module-begin
        (for ([r (in-range 12)])
          (hash-set! registers r 0))
        (run (list LINE ...)))]))
