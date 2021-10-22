#lang racket


(require parser-tools/lex)
(require (only-in esAsm/parser
                  esAsm-program
                  esAsm-program-ops
                  esAsm-program-labels))

(provide parse-moji lex-moji)


;(struct esAsm-program (ops labels))

(define keycaps
  '( "0️⃣"
     "1️⃣"
     "2️⃣"
     "3️⃣"
     "4️⃣"
     "5️⃣"
     "6️⃣"
     "7️⃣"
     "8️⃣"
     "9️⃣"
     ))

(define opcode-assoc
  '(("🖨" shn)
    ;"sha"
    ("⬅" mov)
    ("➕" add)
    ("➖" sub)
    ("✈️" jmp)
    ("✈️👎" jlt)
    ("✈👍" jgt)
    ("✈👌" jet)
    ("✍️" inp)
    ("✋" hlt)
    ))

(define reg-assoc
  '(("💖" 0)
    ("❤️"  1)
    ("🧡" 2)
    ("💛" 3)
    ("💚" 4)
    ("💙" 5)
    ("💜" 6)
    ("🖤" 7)))

(define-lex-abbrevs
  [digit (union "0️⃣"
                "1️⃣"
                "2️⃣"
                "3️⃣"
                "4️⃣"
                "5️⃣"
                "6️⃣"
                "7️⃣"
                "8️⃣"
                "9️⃣")]
  [integer
   (repetition 1 +inf.0 digit)]
  [literal
   (concatenation "#️⃣" integer)]
  [register
   (union "💖"
          "❤️"
          "🧡"
          "💛"
          "💚"
          "💙"
          "💜"
          "🖤")]
  [value
   (union integer register)]
  [instruction
   (union "🖨";"shn"
          ;"sha"
          "⬅";"mov"
          "➕";"add"
          "➖";"sub"
          "✈️";"jmp"
          "✈️👎";"jlt"
          "✈👍";"jgt"
          "✈👌";"jet"
          "✍️";"inp"
          "✋";"hlt"
          )]
  [label
   (repetition 1 +inf.0 (intersection (union "🌁"
                                             "🌃"
                                             "🏙️"
                                             "🌄"
                                             "🌅"
                                             "🌆"
                                             "🌉")
                                      (complement (union "👈" space newline))))]
  [labelMark
   (concatenation label "👈")]
  [newline #\newline]
  [comment (concatenation #\; (repetition 0 +inf.0 (intersection (complement newline) any-char)) newline)]
  [hashlang (concatenation "#lang esAsm" newline)]
  [space #\space])

(define (every-third lst)
  (if (null? lst)
      '() 
      (cons (first lst) 
            (every-third (if (< (length lst) 3)
                             '()
                             (cdddr lst))))))

(define (val-to-num val)
  (string->number (list->string (rest (every-third (string->list val))))))
;(string->number (list->string (rest (string->list val)))))


(define lex-moji
  (lexer
   [instruction (cons `(OPCODE ,lexeme) (lex-moji input-port))]
   [literal (cons `(INT ,(val-to-num lexeme)) (lex-moji input-port))]
   [register (cons `(REGISTER ,lexeme) (lex-moji input-port))]
   [label (cons `(LABEL ,lexeme) (lex-moji input-port))]
   [labelMark (cons `(LABEL-MARK ,(substring lexeme 0 (- (string-length lexeme) 1))) (lex-moji input-port))]
   [newline (cons `(NEWLINE) (lex-moji input-port))]
   [space (lex-moji input-port)]
   [comment (lex-moji input-port)]
   [hashlang (lex-moji input-port)]
   [(eof) '()]
   ))


(define (parse-moji in)
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
                           [(REGISTER) `(REG-REF ,(second (assoc val reg-assoc)))])])
             (if (equal? '(NEWLINE) (first (rest expr-aux)))
                 `(,result)
                 (cons result (parse-aux (rest expr-aux) labels next-idx))))]

          ['(NEWLINE) (parse-aux (rest expr-aux) labels idx) ]
          [`(LABEL-MARK ,label) (parse-aux (rest expr-aux) (cons (list label idx) labels) idx)]

          [`(OPCODE ,op) (if (empty? (rest (member '(NEWLINE) expr-aux)))
                             (esAsm-program (cons (cons (second (assoc op opcode-assoc))
                                                        (parse-aux (rest expr-aux) labels next-idx)) '())
                                            labels)
                             
                             (let ([rec-prog (parse-aux (rest (member '(NEWLINE) expr-aux)) labels next-idx)])
                               (esAsm-program
                                (cons (cons (second (assoc op opcode-assoc))
                                            (parse-aux (rest expr-aux) labels next-idx))
                                      (esAsm-program-ops rec-prog))
                                (esAsm-program-labels rec-prog))))]
          [EOF (esAsm-program expr-aux labels)])))
  (parse-aux (lex-moji in) '() 0))

;(esAsm-program-ops (parse-moji
;                   (open-input-string "🖨#️⃣4️⃣\n")))
