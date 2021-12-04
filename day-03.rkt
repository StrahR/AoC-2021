#lang racket

(require "muf-lib.rkt")

(define day "03")

(define input (map binstr->number (aoc-read day)))
(define len 12)

(define (dominant input k)
  (if (null? (cdr input)) (values -1 input input)
      (let-values ([(1s 0s) (partition (λ (n) (< 0 (bitmask n k))) input)])
        (if (< (length 1s) (length 0s))
            (values 0 0s 1s)
            (values 1 1s 0s)))))

(define (nal1 input)
  (for/fold ([gam 0] [eps 0] #:result (* gam eps))
            ([i (in-range len)])
    (if (zero? (value-ref (dominant input i) 0))
        (values gam (+ eps (expt 2 i)))
        (values (+ gam (expt 2 i)) eps))))

(define (nal2 input)
  (for/fold ([gams input] [epss input] #:result (* (car epss) (car gams)))
            ([i (in-range (sub1 len) -1 -1)])
    (values (value-ref (dominant gams i) 1)
            (value-ref (dominant epss i) 2))))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
