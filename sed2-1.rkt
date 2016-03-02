#lang racket

(require parser-tools/lex
         parser-tools/lex-sre
         parser-tools/yacc)

(define-tokens a (NUM RE GLOBAL-RE ACTION))
(define-empty-tokens b (- EOF < \; \,))

(define-lex-abbrev digit (char-range "0" "9"))
(define-lex-abbrev action (union "c" "dc" "l" "dl"))
(define-lex-abbrev mre (seq "/" (+ (char-complement "/")) "/"))

(define lexer1
  (lexer
   ["-" (token--)]
   [";" (token-\;)]
   ["<" (token-<)]
   ["," (token-\,)]
   [(+ (char-range "0" "9")) (token-NUM lexeme)]
   [(union "c" "dc" "l" "dl") (token-ACTION lexeme)]
   [(seq "/" (+ (char-complement "/")) "/g") (token-GLOBAL-RE lexeme)]
   [(seq "/" (+ (char-complement "/")) "/") (token-RE lexeme)]
   [(eof) (token-EOF)]
   [whitespace (lexer1 input-port)]))

(define-struct range-exp (from to))
(define-struct location-exp (l))
(define-struct global-re-exp (r))
(define-struct re-exp (r))
(define-struct command-exp (action locations))
(define-struct action-exp (action))
(define-struct num-exp (n))

(define parser1
  (parser
   (start commands)
   (end EOF)
   (error (lambda args (map displayln args)))
   (tokens a b)
   (grammar
    (commands ((command \; commands) (cons $1 $3))
              ((command) (list $1)))
    (command ((ACTION region) (command-exp (action-exp $1) $2)))
    (region ((location \, region) (cons $1 $3))
            ((location) (list $1)))
    (location ((place) $1)
              ((GLOBAL-RE) (global-re-exp $1))
              ((- place) (range-exp 'start $2))
              ((place -) (range-exp $1 'end))
              ((place - place) (range-exp $1 $3)))
    (place ((NUM) (num-exp $1))
           ((RE) (re-exp $1))))))

; expand commands so that each one has only one location
(define commands
  (let* ([input (open-input-string "c5,6,/re/")])
    (parser1 (lambda () (lexer1 input)))))

; are any of the commands about column regexps?
(define need-headers
  (ormap (match-lambda
           [(command-exp (or (action-exp "c") (action-exp "dc"))
                         (or (re-exp _)
                             (range-exp (re-exp _) _)
                             (range-exp _ (re-exp _)))) #t]
           [else #f]) commands))