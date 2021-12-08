#lang racket

(require "muf-lib.rkt")

(define day "08")

(define input
  (let ([input (map (cut string-split <> "|") (aoc-read day))])
    (map (cut map string-split <>) input)))

(define (nal1 input)
  (let* ([input (map cadr input)]
         [input (map (cut map string-length <>) input)])
    (count (cut member <> '(2 3 4 7)) (flatten input))))

(aoc-write day 1 (nal1 input))
