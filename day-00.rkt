#lang racket

(require "muf-lib.rkt")

(define day "00")

(define input (aoc-read day))

(define nal1 input)

(define nal2
  (reverse (map string-upcase input)))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
