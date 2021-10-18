#lang racket

(require "parser.rkt")

(provide (rename-out [module-begin-esAsm #%module-begin]))


(define-syntax-rule (module-begin-esAsm body)
  (#%plain-module-begin
   (interp-esAsm 'body)))

(define (uh-oh)
  (error ":("))



(define (interp-esAsm prog)
  (define (interp-aux expr-aux regs)
    (define arg-vals (unless (empty? expr-aux) (rest (first expr-aux))))
    (define (resolve-label label label-list)
      (if (equal? (first (first label-list)) (second label))
          (list-tail (esAsm-program-ops prog) (second (first label-list)))
          (resolve-label label (rest label-list))))
    (define (resolve-val val)
      (case (first val)
        [(REG-REF) (list-ref regs (second val))]
        [(INT) (second val)]))
    (if (empty? expr-aux)
        (interp-aux (esAsm-program-ops prog) regs)
        (case (first (first expr-aux))
          [(shn) (displayln (resolve-val (first arg-vals)))
                 (interp-aux (rest expr-aux) regs)]
          [(inp) (interp-aux (rest expr-aux)
                             (list-set regs (second (first arg-vals)) (read)))]
          [(add) (interp-aux (rest expr-aux)
                             (list-set regs (second (first arg-vals))
                                       (+ (resolve-val (second arg-vals))
                                          (resolve-val (third arg-vals)))))]
          [(mov) (interp-aux (rest expr-aux)
                             (list-set regs (second (first arg-vals))
                                       (resolve-val (second arg-vals))))]
          
          [(jet) (interp-aux (if (= (resolve-val (second arg-vals))
                                    (resolve-val (third arg-vals)))
                                 (resolve-label (first arg-vals) (esAsm-program-labels prog))
                                 (rest expr-aux))
                             regs)]
          [(jmp) (interp-aux (resolve-label (first arg-vals) (esAsm-program-labels prog))
                             regs)]
          
          [(hlt) '()]
          [else (error "uninterp:" (first expr-aux)) (interp-aux (rest expr-aux) regs)])))
  (interp-aux (esAsm-program-ops prog) (make-list 12 0)))
  

;(interp-esAsm
 ;(parse-esAsm (open-input-file "/Users/a11ce/t2.rkt"))

;