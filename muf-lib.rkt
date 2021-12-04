#lang racket

(provide aoc-read aoc-write mapb bool->number)

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