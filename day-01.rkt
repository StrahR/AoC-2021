#lang racket

(include "muf-lib.rkt")

(define day "01")

(define input
  (map string->number (aoc-read day)))

(define (nal1 ll)
  (count identity (mapb < ll (cdr ll))))

(define (nal2 ll)
  (nal1 (mapb + ll (cdr ll) (cddr ll))))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
