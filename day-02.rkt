#lang at-exp racket

(require "muf-lib.rkt")
(require infix)

(define day "02")

(define input
  (map (λ (s) (cons (car s) (string->number (cadr s))))
       (map (λ (s) (string-split s " "))
            (aoc-read day))))

(define (nal1 input)
  (for/fold/match (in-list input)
    ([pos 0] [depth 0] #:result (* pos depth))
    [(cons "forward" n) (values (+ pos n) depth      )]
    [(cons "down" n)    (values pos       (+ depth n))]
    [(cons "up" n)      (values pos       (- depth n))]))

(define (nal2 input)
  (for/fold/match (in-list input)
    ([pos 0] [depth 0] [aim 0] #:result (* pos depth))
    [(cons "forward" n) (values (+ pos n) @${ aim*n + depth } aim      )]
    [(cons "down" n)    (values pos       depth               (+ aim n))]
    [(cons "up" n)      (values pos       depth               (- aim n))]))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
