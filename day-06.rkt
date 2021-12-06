#lang racket

(require "muf-lib.rkt")

(define day "06")

(define input (map string->number (string-split (car (aoc-read day)) ",")))

(define (lanternfish-day input)
  (let ([n (count (cut = 0 <>) input)])
    (append (make-list n 8) (map (λ (n) (if (< n 0) 6 n)) (map sub1 input)))))

(define (nal1 input)
  (for/fold ([lanternfish input] #:result (length lanternfish))
            ([i (in-range 80)])
    (lanternfish-day lanternfish)))

(define (nal2 input) input)

(aoc-write day 1 (nal1 input))
