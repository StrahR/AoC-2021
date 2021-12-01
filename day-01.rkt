#lang racket

(require "muf-lib.rkt")

(define day "01")

(define input
  (map string->number (aoc-read day)))

(define (nal1 input)
  (count identity (mapb < input (cdr input))))

(define (nal2 input)
  (nal1 (mapb + input (cdr input) (cddr input))))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
