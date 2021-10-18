#lang info
(define collection "esAsm")
(define version "1.0")

(define deps '("base"
               "parser-tools-lib"))


(define build-deps '("racket-doc"
                     "scribble-lib"))
               
(define scribblings '(("scribblings/esAsm.scrbl")))
