#lang racket

(require "muf-lib.rkt")

(define day "06")

(define input (map string->number (string-split (car (aoc-read day)) ",")))

(define (get-lanternfish-state input)
  (for/fold ([acc (hash)]) ([f (in-list input)])
    (hash-update acc f add1 0)))

(define (lanternfish-evolution input days)
  (for/fold ([lanternfish (get-lanternfish-state input)]
             #:result (apply + (hash-values lanternfish)))
            ([i (in-range days)])
    (let* ([n (hash-ref lanternfish i 0)]
           [lanternfish (hash-update lanternfish (+ i 7) (cut + <> n) 0)]
           [lanternfish (hash-update lanternfish (+ i 9) (cut + <> n) 0)])
      (hash-remove lanternfish i))))

(aoc-write day 1 (lanternfish-evolution input  80))
(aoc-write day 2 (lanternfish-evolution input 256))
