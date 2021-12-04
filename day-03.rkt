#lang racket

(require "muf-lib.rkt")

(define day "03")

(define (binstr->number binstr)
  (string->number (format "#b~a" binstr)))

(define input (map binstr->number (aoc-read day)))
(define len 12)

; (displayln input)

(define (bitmask n k) (bitwise-and n (expt 2 k)))

(define (dominant input k)
  (let ([1s (filter (λ (n) (< 0 (bitmask n k))) input)]
        [0s (filter (λ (n) (= 0 (bitmask n k))) input)])
    (if (< (length 1s) (length 0s))
        (values 1 1s 0s)
        (values 0 0s 1s))))

(define (nal1 input)
  (for/fold ([gam 0]
             [eps 0]
             #:result (* gam eps))
            ([i (in-range len)])
    (let-values ([(b gams epss) (dominant input i)])
      (if (= b 1)
          (values (+ gam (expt 2 i)) eps)
          (values gam (+ eps (expt 2 i)))))))

(aoc-write day 1 (nal1 input))
