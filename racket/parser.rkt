#lang racket


(require parser-tools/lex)

(provide parse-esAsm lex-esAsm (struct-out esAsm-program))


(struct esAsm-program (ops labels))

(define-lex-abbrevs
  [digit
   (char-range #\0 #\9)]
  [positive-int
   (repetition 1 +inf.0 digit)]
  [integer
   (union (concatenation #\- positive-int)
          positive-int)]
  [literal
   (concatenation #\# integer)]
  [register
   (concatenation #\r positive-int)]
  [value
   (union integer register)]
  [instruction
   (union "shn" "sha" "mov" "add" "sub" "jmp" "jlt" "jgt" "jet" "inp" "hlt" "mficofsr")]
  [label
   (repetition 1 +inf.0 (intersection (union alphabetic numeric)
                                      (complement (union #\: space newline))))]
  [labelMark
   (concatenation label #\:)]
  [newline #\newline]
  [comment (concatenation #\; (repetition 0 +inf.0 (intersection (complement newline) any-char)) newline)]
  [hashlang (concatenation "#lang esAsm" newline)]
  [space #\space])

(define (val-to-num val)
  (string->number (list->string (rest (string->list val)))))


(define lex-esAsm
  (lexer
   [instruction (cons `(OPCODE ,lexeme) (lex-esAsm input-port))]
   [literal (cons `(INT ,(val-to-num lexeme)) (lex-esAsm input-port))]
   [register (cons `(REGISTER ,lexeme) (lex-esAsm input-port))]
   [label (cons `(LABEL ,lexeme) (lex-esAsm input-port))]
   [labelMark (cons `(LABEL-MARK ,(substring lexeme 0 (- (string-length lexeme) 1))) (lex-esAsm input-port))]
   [newline (cons `(NEWLINE) (lex-esAsm input-port))]
   [space (lex-esAsm input-port)]
   [comment (lex-esAsm input-port)]
   [hashlang (lex-esAsm input-port)]
   [(eof) '()]
   ))


(define (parse-esAsm in)
  (define (parse-aux expr-aux labels idx)
    
    (define next-idx (+ 1 idx))
    (if (or (empty? expr-aux)
            (eof-object? (first expr-aux)))
        '()
        (match (first expr-aux)
          [(or `(INT ,val)
               `(LABEL ,val)
               `(REGISTER ,val))
           (let ([result (case (first (first expr-aux))
                           [(INT) (first expr-aux)]
                           [(LABEL) `(LABEL-REF ,val)]
                           [(REGISTER) `(REG-REF ,(val-to-num val))])])
             (if (equal? '(NEWLINE) (first (rest expr-aux)))
                 `(,result)
                 (cons result (parse-aux (rest expr-aux) labels next-idx))))]

          ['(NEWLINE) (parse-aux (rest expr-aux) labels idx) ]
          [`(LABEL-MARK ,label) (parse-aux (rest expr-aux) (cons (list label idx) labels) idx)]

          [`(OPCODE ,op) (if (empty? (rest (member '(NEWLINE) expr-aux)))
                             (esAsm-program (cons (cons (string->symbol op)
                                                        (parse-aux (rest expr-aux) labels next-idx)) '())
                                            labels)
                             
                             (let ([rec-prog (parse-aux (rest (member '(NEWLINE) expr-aux)) labels next-idx)])
                               (esAsm-program
                                (cons (cons (string->symbol op)
                                            (parse-aux (rest expr-aux) labels next-idx))
                                      (esAsm-program-ops rec-prog))
                                (esAsm-program-labels rec-prog))))]
          [EOF (esAsm-program expr-aux labels)])))
  (parse-aux (lex-esAsm in) '() 0))
;(parse-aux in '() 0))


#|
(lex-esAsm (open-input-string
            "; initial values
mov r0 #1
mov r1 #1
shn #1
shn #1
loopStart:
; add to find next term
add r2 r0 r1

; show result
shn r2

; shift previous terms
mov r0 r1
mov r1 r2

; increment term counter
add r3 r3 #1
; stop at term 10 (including two initial)
jet end r3 #10
jet end #10 r3
; loop if not stopping
jmp loopStart
end:
hlt
")) |#