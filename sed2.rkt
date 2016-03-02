#lang racket

#|
Need a bunch of stuff:
- argf commandline handling
- better parsing of the commands, e.g., c1,3-4
- support for regex matching, e.g., c/foo/
- column thinking that happens at the start (or first line)
|#

(require racket/cmdline)

(define separator (make-parameter "\t"))

(define-values (commands fns)
  (command-line
   #:program "sed2"
   #:once-each
   [("-F" "--separator") sep
                         "Column separator"
                         (separator sep)]
   #:args (commands . fns)
   (values commands fns)))

;(define command-raw "c2-; l2-4")

(define lines
  (call-with-input-file (first fns)
    (lambda (inp)
      (sequence->list (in-lines inp)))))

(define (split-columns str)
  (string-split str (separator)))

(define (join-columns lst)
  (string-join lst (separator)))

(define (get-columns-lambda #:from [from 'start] #:to [to 'end])
  (lambda (str line-number)
    (let ([cols (split-columns str)])
      (join-columns
       (match* (from to)
         [('start 'end) cols]
         [('start _) (take cols (string->number to))]
         [(_ 'end) (drop cols (sub1 (string->number from)))]
         [(_ _) (drop (take cols (string->number to)) (sub1 (string->number from)))])))))

(define (get-lines-lambda #:from [from 'start] #:to [to 'end])
  (lambda (str line-number)
    (match* (from to)
      [('start 'end) str]
      [('start _) (if (<= line-number (string->number to))
                      str
                      (void))]
      [(_ 'end) (if (>= line-number (string->number from))
                    str
                    (void))]
      [(_ _) (if (and (>= line-number (string->number from)) (<= line-number (string->number to)))
                 str
                 (void))])))
           

(define/match (parse-action action)
  [((regexp #px"c(\\d+)-(\\d+)")) (let* ([posns (rest (regexp-match #px"c(\\d+)-(\\d+)" action))]
                                         [from (first posns)]
                                         [to (second posns)])
                                    (get-columns-lambda #:from from #:to to))]
  [((regexp #px"c(\\d+)-")) (let ([from (second (regexp-match #px"c(\\d+)-" action))])
                              (get-columns-lambda #:from from))]
  [((regexp #px"l(\\d+)-(\\d+)")) (let* ([posns (rest (regexp-match #px"l(\\d+)-(\\d+)" action))]
                                         [from (first posns)]
                                         [to (second posns)])
                                    (get-lines-lambda #:from from #:to to))])

(let* ([command-parts (map string-trim (string-split commands ";"))]
       [actions (map parse-action command-parts)])
  (for ([line lines]
        [line-number (in-naturals 1)])
    (let ([out (for/fold ([out line])
                         ([action actions])
                 (action out line-number))])
      (cond
        [(void? out) (void)]
        [(string? out) (displayln out)]))))
  