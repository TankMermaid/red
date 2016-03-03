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
    (place ((NUM) (num-exp (string->number $1)))
           ((RE) (re-exp $1))))))

(define commands
  (let* ([input (open-input-string "c3-/re/; dc2")])
    (parser1 (lambda () (lexer1 input)))))

; are any of the commands about column regexps?
(define need-headers
  (ormap (match-lambda
           [(command-exp (or (action-exp "c") (action-exp "dc"))
                         (or (re-exp _)
                             (range-exp (re-exp _) _)
                             (range-exp _ (re-exp _)))) #t]
           [else #f]) commands))

(define separator (make-parameter "\t"))

(define (string->regexp r)
  (regexp (substring r 1 (sub1 (string-length r)))))

(define (columns-match locations first-line)
  (let* ([headers (string-split first-line (separator))]
         [n-columns (length headers)]
         [column-matches (make-vector n-columns #f)]
         [loc-match (lambda (location)
                      (match location
                        [(num-exp n) (sub1 n)]
                        [(re-exp r)(for/first ([i (in-range n-columns)]
                                                #:when (regexp-match (string->regexp r) (list-ref headers i)))
                                      i)]))])
    (for ([location locations])
      (match location
        [(or (num-exp _) (re-exp _)) (vector-set! column-matches (loc-match location) #t)]
        [(range-exp 'start end) (let ([end-i (loc-match end)])
                                  (for ([i (in-range (add1 end-i))])
                                    (vector-set! column-matches i #t)))]
        [(range-exp start 'end) (let ([start-i (loc-match start)])
                                  (for ([i (in-range start-i n-columns)])
                                    (vector-set! column-matches i #t)))]
        [(range-exp start end) (let ([start-i (loc-match start)]
                                     [end-i (loc-match end)])
                                 (for ([i (in-range start-i (add1 end-i))])
                                   (vector-set! column-matches i #t)))]))
    (vector->list column-matches)))

(define (columns-nonmatch locations first-line)
  (map not (columns-match locations first-line)))

(define (take-column-lambda matches)
  (lambda (line)
    (string-join
     (let ([fields (string-split line (separator))])
       (for/list ([field fields]
                  [m matches]
                  #:when m)
         field)) (separator))))

(define (drop-column-lambda matches)
  (lambda (line)
    (string-join
     (let ([fields (string-split line (separator))])
       (for/list ([field fields]
                  [m matches]
                  #:when (not m))
         field)) (separator))))

; convert a command into a function (of the line content only)
(define (command->lambda command first-line)
  (match command
    [(command-exp (action-exp "c") locs) (take-column-lambda (columns-match locs first-line))]
    [(command-exp (action-exp "dc") locs) (drop-column-lambda (columns-match locs first-line))]))

; chain together commands into a single function (of line content)
(define (commands->lambda commands first-line)
  (apply compose1 (reverse (map (λ (cmd) (command->lambda cmd first-line)) commands))))

((commands->lambda commands "foo\tbar\tbaz\tqux\treal_big_fish")
 "foo\tbar\tbaz\tqux\treal_big_fish")