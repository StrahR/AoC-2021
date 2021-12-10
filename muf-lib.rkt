#lang racket

(require srfi/26) ; Cut macro for partial function application
(require infix)

(require (for-syntax syntax/for-body))

(define (aoc-read day [test #f])
  (let ([filename (format (if test "day-~a-test.in" "day-~a.in") day)])
    (if (file-exists? filename)
        (file->lines filename)
        (begin (system (format "raco aoc -y 2021 -d ~a > ~a" day filename))
               (aoc-read day test)))))

(define (aoc-write day part soln [sub #f])
  (begin
    (display-to-file (~a soln) (format "day-~a-~a.out" day part) #:exists 'truncate)
    (cond [sub (system (format "raco aoc -y 2021 -d ~a -a ~a ~a" day part soln))])))

(define (mapb f . xss)
  (define (aux acc . xss)
    (if (ormap empty? xss)
        (reverse acc)
        (apply aux (cons (apply f (map car xss)) acc) (map cdr xss))))
  (apply aux '() xss))

(define (bool->number b) (if b 1 0))

(define-syntax-rule (value-ref v n)
  (list-ref (call-with-values (λ () v) list) n))

(define (binstr->number binstr)
  (string->number (format "#b~a" binstr)))

(define (bitmask n k) (bitwise-and n (expt 2 k)))

(define-syntax (for/fold/match stx)
  (syntax-case stx ()
    [(_ iterable acc body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           acc
           ([iterator iterable])
           pre-body ...
           (let () (match iterator post-body ...))))]))

(define-syntax (for/sum/match stx)
  (syntax-case stx ()
    [(_ iterable body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           ([acc 0])
           ([iterator iterable])
           pre-body ...
           (+ acc (let () (match iterator post-body ...)))))]))

(define-syntax (for*/filter/list stx)
  (syntax-case stx ()
    [(_ iter body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for*/fold/derived original
           ([acc '()] #:result (reverse (filter-not void? acc)))
           iter
           pre-body ...
           (cons (let () post-body ...) acc)))]))

(define-syntax-rule (mλ body) (match-lambda body))

(define (list-dedup xs)
  (for/fold ([acc '()] #:result (reverse acc))
            ([x (in-list xs)])
    (cond [(empty? acc) (list x)]
          [(equal? (car acc) x) acc]
          [else (cons x acc)])))

(provide (all-defined-out) cut)
