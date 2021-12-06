#lang racket

(require "muf-lib.rkt")

(define day "06")

(define input
  (let ([input (map string->number (string-split (car (aoc-read day)) ","))]
        [acc (make-hash)])
    (for ([f (in-list input)])
      (hash-update! acc f add1 0))
    acc))

(define (lanternfish-evolution lanternfish days)
  (for ([i (in-range days)])
    (let ([n (hash-ref! lanternfish i 0)])
      (hash-update! lanternfish (+ i 7) (cut + <> n) 0)
      (hash-update! lanternfish (+ i 9) (cut + <> n) 0)
      (hash-remove! lanternfish i)))
  (apply + (hash-values lanternfish)))

(aoc-write day 1 (lanternfish-evolution (hash-copy input)  80))
(aoc-write day 2 (lanternfish-evolution (hash-copy input) 256))
