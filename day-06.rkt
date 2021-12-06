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

(define (get-lanternfish h d)
  (flatten (for/list ([f (in-hash-pairs h)])
             (match f [(cons key value) (make-list value (- key d -1))]))))

(define (nal2 input)
  (let ([input (for/fold ([acc (hash)])
                         ([f (in-list input)])
                 (hash-update acc f add1 0))])
    (for/fold ([lanternfish input] #:result (apply + (hash-values lanternfish)))
              ([i (in-range 256)])
      (let* ([n (hash-ref lanternfish i 0)]
             [lanternfish (hash-set lanternfish i 0)]
             [lanternfish (hash-update lanternfish (+ i 7) (cut + <> n) 0)]
             [lanternfish (hash-update lanternfish (+ i 9) (cut + <> n) 0)])
        lanternfish))))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
