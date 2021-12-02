#lang racket

(require "muf-lib.rkt")

(define day "02")

(define input
  (map (λ (s) (cons (car s) (string->number (cadr s))))
       (map (λ (s) (string-split s " "))
            (aoc-read day))))

(define (nal1 input)
  (for/fold ([pos 0] [depth 0] #:result (* pos depth))
            ([command (in-list input)])
    (match command
      [(cons "forward" n) (values (+ pos n) depth)]
      [(cons "down" n) (values pos (+ depth n))]
      [(cons "up" n) (values pos (- depth n))])))

(define (nal2 input)
  (for/fold ([aim 0] [pos 0] [depth 0] #:result (* pos depth))
            ([command (in-list input)])
    (match command
      [(cons "forward" n) (values aim (+ pos n) (+ (* aim n) depth))]
      [(cons "down" n) (values (+ aim n) pos depth)]
      [(cons "up" n) (values (- aim n) pos depth)])))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
