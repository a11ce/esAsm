#lang racket

(require "lexer.rkt")

(define (resolve val)
  (match val
    [(list 'num n) n]
    [(list 'reg r) (format "r~a" r)]))

(define (compile-inst inst)
  (match inst
    [(list 'SHN val)
     (format "printf(\"%d\\n\", ~a)"
             (resolve val))]
    [(list 'SHA val)
     (format "printf(\"%c\\n\", ~a)"
             (resolve val))]
    [(list 'MOV reg val)
     (format "r~a = ~a" reg (resolve val))]
    [(list 'ADD reg val1 val2)
     (format "r~a = ~a + ~a" reg
             (resolve val1)
             (resolve val2))]
    [(list 'SUB reg val1 val2)
     (format "r~a = ~a - ~a" reg
             (resolve val1)
             (resolve val2))]
    [(list 'JMP lab)
     (format "goto ~a" lab)]
    [(list 'JLT lab val1 val2)
     (format "if (~a < ~a){goto ~a;}"
             (resolve val1)
             (resolve val2)
             lab)]
    [(list 'JGT lab val1 val2)
     (format "if (~a > ~a){goto ~a;}"
             (resolve val1)
             (resolve val2)
             lab)]
    [(list 'JET lab val1 val2)
     (format "if (~a == ~a){goto ~a;}"
             (resolve val1)
             (resolve val2)
             lab)]
    [(list 'HLT)
     "return 0"]
    [(list 'MFICOFSR reg)
     (format "r~a = 0" reg)]
    [x (error 'compiler
              (format "what is ~v"
                      inst))]))

(define (compile prog)
  (define op (open-output-string))
  (parameterize ([current-output-port op])
    ; header
    (printf "#include <stdio.h>\nint main()\n{\n")
    ; register setup
    (printf "int r0 = 0")
    (for ([idx (in-range 1 12)]) 
      (printf ", r~a = 0" idx))
    (printf ";\n")
    ; translate labels/instructions
    (map (match-lambda
           [(list 'lab-decl lab-name)
            (printf "~a:\n" lab-name)]
           [(list-rest 'inst args)
            (printf "~a;\n"
                    (compile-inst args))]
           [else '()])
         prog)
    ; close main
    (printf "}"))
  (get-output-string op))

(define (esAsm->c ip)
  (compile (parse-esAsm-to-datum ip)))

#;(display (esAsm->c 
            (open-input-file "examples/fibonacci.es.rkt")))


;;; sublanguage stuff

(require (for-syntax syntax/parse)
         syntax/strip-context)

(provide #%top #%app #%datum #%top-interaction
         (rename-out [esAsm-mod-begin #%module-begin]))

(provide read-syntax)

(define (read-syntax path port)
  (define ast
    (parse-esAsm port))
  (strip-context
   #`(module esAsm-mod esAsm/c-compile
       #,ast)))

(define-syntax (esAsm-mod-begin stx)
  (syntax-parse stx
    [(esAsm-mod-begin (program LINE ...))
     #'(#%module-begin
        (displayln
         (compile (list
                   'LINE ...))))]))

(module+ reader
  (provide read-syntax))
