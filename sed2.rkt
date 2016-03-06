#lang racket

(require parser-tools/lex
         (prefix-in sre: parser-tools/lex-sre)
         parser-tools/yacc)

(define raw-command "c/B/g; l/1/g")
(define fn "/Users/scott/work/racket/z.txt")

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
   [(sre:+ (char-range "0" "9")) (token-NUM lexeme)]
   [(union "c" "dc" "l" "dl") (token-ACTION lexeme)]
   [(sre:seq "/" (sre:+ (char-complement "/")) "/g") (token-GLOBAL-RE lexeme)]
   [(sre:seq "/" (sre:+ (char-complement "/")) "/") (token-RE lexeme)]
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
  (let* ([input (open-input-string raw-command)])
    (parser1 (lambda () (lexer1 input)))))

(define separator (make-parameter "\t"))

(define (string->regexp r)
  (regexp (substring r 1 (sub1 (string-length r)))))

(define (string->global-regexp r)
  (regexp (substring r 1 (- (string-length r) 2))))

(define (columns-match locations first-line)
  (let* ([headers (string-split first-line (separator))]
         [n-columns (length headers)]
         [column-matches (make-vector n-columns #f)]
         [loc-match (lambda (location)
                      (match location
                        [(num-exp n) (sub1 n)]
                        [(re-exp r) (length (takef headers (λ (h) (not (regexp-match (string->regexp r) h)))))]))])
    (for ([location locations])
      (match location
        [(or (num-exp _) (re-exp _)) (vector-set! column-matches (loc-match location) #t)]
        [(global-re-exp r) (let ([re (string->global-regexp r)])
                             (for ([i (in-range n-columns)]
                                   [h headers])                               
                               (when (regexp-match re h)
                                 (vector-set! column-matches i #t))))]
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

(define (take-column-lambda matches)
  (lambda (line)
    (string-join
     (let ([fields (string-split line (separator))])
       (for/list ([field fields]
                  [m matches]
                  #:when m)
         field)) (separator))))

(define column-editor%
  (class object%
    (init locs [invert? #f])
    (define _locs locs)
    (define _invert? invert?)
    (define line-action (void))
    (define first-line? #t)
    (super-new)
    (define/public (process line)
      ; define the line action
      (when first-line?
        (set! first-line? #f)
        (let ([cols (columns-match _locs line)])
          (if _invert?
              (set! line-action (take-column-lambda (map not cols)))
              (set! line-action (take-column-lambda cols)))))
      ; perform line action
      (line-action line))))

(define line-range%
  (class object%
    (init lower upper [started? #f])
    (define _lower
      (match lower
        [(re-exp r) (string->regexp r)]
        [(num-exp n) n]))
    (define _upper
      (match upper
        [(re-exp r) (string->regexp r)]
        [(num-exp n) n]))
    (define _started? started?)
    (define done? #f)
    (super-new)
    (define/public (in-range? line line-number)
      (if done?
          #f
          (let ([at-lower? (or (and (regexp? _lower) (regexp-match _lower line))
                               (and (number? _lower) (equal? _lower line-number)))]
                [at-upper? (or (and (regexp? _upper) (regexp-match _upper line))
                               (and (number? _upper) (equal? _upper line-number)))])
            (when at-lower? (set! _started? #t))
            (when at-upper? (set! done? #t))
            _started?)))))

(define global-re-line%
  (class object%
    (init re)
    (define _re re)
    (super-new)
    (define/public (in-range? line line-number)
      (regexp-match _re line))))

(define line-keeper%
  (class object%
    (init locs [invert? #f])
    (define _invert? invert?)
    (define ranges
      (map (λ (loc)
             (match loc
               [(or (num-exp _) (re-exp _)) (new line-range% [lower loc] [upper loc])]
               [(range-exp 'start upper) (new line-range% [lower 1] [upper upper] [started? #t])]
               [(range-exp lower 'end) (new line-range% [lower lower] [upper +inf.0])]
               [(range-exp lower upper) (new line-range% [lower lower] [upper upper])]
               [(global-re-exp r) (new global-re-line% [re (string->global-regexp r)])])) locs))
    (super-new)
    (define line-number 0)
    (define/public (process line)
      (set! line-number (add1 line-number))
      (if (xor _invert? (ormap (λ (range) (send range in-range? line line-number)) ranges))
          line
          #f))))
    

(define command->object
  (match-lambda
    [(command-exp (action-exp "c") locs) (new column-editor% [locs locs])]
    [(command-exp (action-exp "dc") locs) (new column-editor% [locs locs] [invert? #t])]
    [(command-exp (action-exp "l") locs) (new line-keeper% [locs locs])]
    [(command-exp (action-exp "dl") locs) (new line-keeper% [locs locs] [invert? #t])]))

(define command-objects (map command->object commands))

(call-with-input-file fn
  (λ (out) (for ([line (in-lines out)])
             (let ([out-line (for/fold ([l line])
                                       ([obj command-objects])
                               #:break (not l)
                               (send obj process line))])
               (when out-line
                 (displayln out-line))))))