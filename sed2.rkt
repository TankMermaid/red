#lang racket

(require parser-tools/lex
         (prefix-in sre: parser-tools/lex-sre)
         parser-tools/yacc
         racket/cmdline)

(define separator (make-parameter "\t"))

(define raw-command
  (command-line
   #:program "sed2"
   #:once-each
   [("-F" "--separator") sep
                         "Column separator (default: tab)"
                         (separator sep)]
   #:ps "
sed2 takes a command made up of four actions:
 - l  : keep line
 - dl : drop line
 - c  : keep column
 - dc : drop column
 - s  : substitute

The \"keep\" actions implicitly drop lines or columns that are not
expressly kept.

The line and column actions are accompanied by specifications of
their \"domain\". These specifications can be:
 - numbers like 1
 - regexps like /foo/
 - global regexps like /foo/g

Normal regexps match the first occurrence. Numbers and regexps can
appear in ranges like 1-5 or /foo/-10. Ranges with unspecified lower
range start at 1 (-5 is equivalent to 1-5); unspecified upper ranges
to go the number of columns for lines (l6- is like dl1-5).

When a column is specified with a regexp, sed2 searches for that
pattern in the first line that action sees and figures out which
columns to keep or drop based on that first line.

Column numbers can be counted \"from the end\" by adding <. Thus
you can drop the last two columns with c-3< or dc2<-.

Domains can be joined with commas. l1,3,5 keeps the first, third, and
fifth lines.

Substitutions can use any delimiter, so s/foo/bar/ and s%foo%bar% are
equivalent. s/from/to/ substitutes the first \"from\", s/from/to/g is
a global replacement.

Actions can be chained together with semicolons. Thus
  sed2 \"dc1\" | sed2 \"l1-5\"
should produce the same result as
  sed2 \"dc1; l1-5\"

The first line of input to a column command is special. It uses that
line to determine which columns to keep. Thus \"dl1; c/foo/\" drops
the first line, looks for foo in the second line of the file, and keeps
the column that had foo in the second line of the file, while \"c/foo/; dl1\"
looks for foo in the file's first line, drops that line, and keeps the
column that had foo in the file's first line.

All line counts are done according to what that action saw, not the file
numbers. Thus \"dl1-5; dl1-5\" is equivalent to \"dl1-10\". The point here
is that something like \"l/foo/g; l1-5\" is like grep foo | head -5.

Whitespace in the command string is ignored."
   #:args (cmd)
   cmd))

(define-tokens a (NUM RE GLOBAL-RE ACTION SUB))
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
   [(sre:seq "s/" (sre:+ (char-complement "/")) "/" (sre:+ (char-complement "/")) "/" (sre:* (union "g"))) (token-SUB lexeme)]
   [(eof) (token-EOF)]
   [whitespace (lexer1 input-port)]))

(define-struct range-exp (from to))
(define-struct location-exp (l))
(define-struct global-re-exp (r))
(define-struct re-exp (r))
(define-struct command-exp (action locations))
(define-struct action-exp (action))
(define-struct num-exp (n from-end?))
(define-struct sub-exp (from to flags))

(define parser1
  (parser
   (start commands)
   (end EOF)
   (error (lambda args (map displayln args)))
   (tokens a b)
   (grammar
    (commands ((command \; commands) (cons $1 $3))
              ((command \;) (list $1))
              ((command) (list $1)))
    (command ((ACTION region) (command-exp (action-exp $1) $2))
             ((SUB) (parse-sub-command $1)))
    (region ((location \, region) (cons $1 $3))
            ((location) (list $1)))
    (location ((place) $1)
              ((GLOBAL-RE) (global-re-exp $1))
              ((- place) (range-exp 'start $2))
              ((place -) (range-exp $1 'end))
              ((place - place) (range-exp $1 $3)))
    (place ((NUM) (num-exp (string->number $1) #f))
           ((NUM <) (num-exp (string->number $1) #t))
           ((RE) (re-exp $1))))))

(define (parse-sub-command s)
  (let* ([sep (substring s 1 2)]
         [parts (string-split s sep)]
         [from (second parts)]
         [to (third parts)]
         [flags (if (equal? (length parts) 4) (string->list (fourth parts)) '())])
    (sub-exp from to flags)))
         

(define commands
  (let* ([input (open-input-string raw-command)])
    (parser1 (lambda () (lexer1 input)))))

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
                        [(num-exp n #f) (sub1 n)]
                        [(num-exp n #t) (- n-columns n)]
                        [(re-exp r) (length (takef headers (λ (h) (not (regexp-match (string->regexp r) h)))))]))])
    (for ([location locations])
      (match location
        [(or (num-exp _ _) (re-exp _)) (vector-set! column-matches (loc-match location) #t)]
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
      ; perform line action; always keep
      (list (line-action line) #t))))

(define line-range%
  (class object%
    (init lower upper [started? #f])
    (define _lower
      (match lower
        [(re-exp r) (string->regexp r)]
        [(num-exp n #f) n]
        [(num-exp _ #t) (error "can't count from the end in lines")]))
    (define _upper
      (match upper
        [(re-exp r) (string->regexp r)]
        [(num-exp n #f) n]
        [(num-exp _ #t) (error "can't count from the end in lines")]))
    (define _started? started?)
    (super-new)
    (define/public (in-range? line line-number)
      (let ([at-lower? (or (and (regexp? _lower) (regexp-match _lower line))
                           (and (number? _lower) (equal? _lower line-number)))]
            [at-upper? (or (and (regexp? _upper) (regexp-match _upper line))
                           (and (number? _upper) (equal? _upper line-number)))])
        (cond
          [at-upper? '(#t #f)] ; we're in range and don't keep me
          [at-lower? '(#t #t)] ; in range and keep me
          [else '(#f #t)]))))) ; not in range and keep me

(define global-re-line%
  (class object%
    (init re)
    (define _re re)
    (super-new)
    (define/public (in-range? line line-number)
      ; 
      (list (regexp-match _re line) #t))))

(define (keep-where lst-keep? lst)
  (for/list ([keep? lst-keep?]
             [item lst]
             #:when keep?)
    item))

(define line-keeper%
  (class object%
    (init locs [invert? #f])
    (define _invert? invert?)
    (define ranges
      (map (λ (loc)
             (match loc
               [(or (num-exp _ _) (re-exp _)) (new line-range% [lower loc] [upper loc])]
               [(range-exp 'start upper) (new line-range% [lower 1] [upper upper] [started? #t])]
               [(range-exp lower 'end) (new line-range% [lower lower] [upper +inf.0])]
               [(range-exp lower upper) (new line-range% [lower lower] [upper upper])]
               [(global-re-exp r) (new global-re-line% [re (string->global-regexp r)])])) locs))
    (super-new)
    (define line-number 0)
    (define/public (process line)
      (set! line-number (add1 line-number))
      (let* ([results (map (λ (range) (send range in-range? line line-number)) ranges)]
             ; are we in any of the ranges?
             [in-range? (ormap identity (map first results))]
             ; which of these ranges should I keep?
             [keep-range? (map second results)]
             [keep-me? (ormap identity keep-range?)])
        ; remove done ranges
        (set! ranges (keep-where keep-range? ranges))
        (if in-range?
            (list line keep-me?)
            (list #f keep-me?))))))

(define substitutor%
  (class object%
    (init from to [flags '()])
    (define from-re (regexp from))
    (define _to to)
    (define _flags flags)
    (define (f line)
      (match _flags
        ['() (regexp-replace from-re line _to)]
        [(list-no-order #\g) (regexp-replace* from-re line _to)]
        [else (error (format "unknown flags \"~a\" in substitute command" (list->string _flags)))]))
    (super-new)
    (define/public (process line)
      (list (f line) #t)))) ; always keep

(define command->object
  (match-lambda
    [(command-exp (action-exp "c") locs) (new column-editor% [locs locs])]
    [(command-exp (action-exp "dc") locs) (new column-editor% [locs locs] [invert? #t])]
    [(command-exp (action-exp "l") locs) (new line-keeper% [locs locs])]
    [(command-exp (action-exp "dl") locs) (new line-keeper% [locs locs] [invert? #t])]
    [(sub-exp from to flags) (new substitutor% [from from] [to to] [flags flags])]
    ))

(define command-objects (map command->object commands))

(for ([line (in-lines)])
  #:break (zero? (length command-objects))
  (let ([out-line (for/fold ([l line])
                            ([obj command-objects])
                    #:break (not l)
                    (let* ([res (send obj process l)]
                           [new-line (first res)]
                           [keep-obj? (second res)])
                      (unless keep-obj?
                        (set! command-objects (remove obj command-objects)))
                      new-line))])
    (when out-line
      (displayln out-line))))