#lang racket

(require "muf-lib.rkt")

(define day "07")

(define input (map string->number (string-split (car (aoc-read day)) ",")))

(define (nal1 input)
  (define (fuel-cost input target)
    (apply + (map (λ (n) (abs (- n target))) input)))
  (apply min (for/list ([i (in-range 1000)]) (fuel-cost input i))))

(define (nal2 input)
  (define (fuel-cost input target)
    (apply + (map (λ (n) (apply + (range (add1 (abs (- n target)))))) input)))
  (apply min (for/list ([i (in-range 1000)]) (fuel-cost input i))))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
