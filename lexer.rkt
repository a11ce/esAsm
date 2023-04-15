#lang racket

(require brag/support
         "grammar.rkt")

(provide parse-esAsm
         parse-esAsm-to-datum)

(define-lex-abbrevs
  [label-name
   (:+ alphabetic)]
  [register
   (:: "r" numeric (:? numeric))]
  [literal
   (:: "#" (:+ numeric))]
  [comment
   (:: (:or ";" "#lang esAsm") (:* (:~ "\n")))]
  [instruction
   (:or "shn" "sha" "mov" "add" "sub" "jmp"
        "jlt" "jgt" "jet" "inp" "hlt" "mficofsr" "scof")])

(define esAsm-lexer
  (lexer-src-pos
   [(:: label-name ":")
    (token 'LABEL (substring lexeme 0 (sub1 (string-length lexeme))))]
   [register
    (token 'REG (string->number (substring lexeme 1)))]
   [literal
    (token 'NUM (string->number (substring lexeme 1)))]
   [instruction
    (token (string-upcase lexeme)
           (string->symbol
            (string-upcase lexeme)))]
   [label-name
    (token 'LABEL lexeme)]
   ["\n" (token 'NEWLINE lexeme)]
   [(:or whitespace comment)
    (token 'whitespace-or-comment lexeme #:skip? #t)]))
  
(define (tokenize ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token) (esAsm-lexer ip))
  next-token)

(define (parse-esAsm-to-datum ip)
  (parse-to-datum (tokenize ip)))

(define (parse-esAsm ip)
  (parse (tokenize ip)))

#;(parse-esAsm-to-datum (open-input-file "examples/scof.rkt"))
