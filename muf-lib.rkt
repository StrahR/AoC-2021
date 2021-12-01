; #lang racket

(define (aoc-read day [test #f])
  (let ([filename (format (if test "day-~a-test.in" "day-~a.in") day)])
    (if (file-exists? filename)
        (file->lines filename)
        (begin (system (format "raco aoc -y 2021 -d ~a > ~a" day filename))
               (aoc-read day test)))))

(define (aoc-write day part soln [sub #f])
  (begin
    (call-with-output-file (format "day-~a-~a.out" day part)
      #:exists 'truncate
      (lambda (out) (display (~a soln) out)))
    (cond [sub (system (format "raco aoc -y 2021 -d ~a -a ~a ~a" day part soln))])))

(define (mapb f . xss)
  (define (aux acc . xss)
    (if (ormap empty? xss)
        (reverse acc)
        (apply aux (cons (apply f (map car xss)) acc) (map cdr xss))))
  (apply aux '() xss))

(provide aoc-read aoc-write mapb)
