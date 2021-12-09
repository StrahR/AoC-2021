#lang racket

(require "muf-lib.rkt")

(define day "09")

(define input (map (compose (curry map (λ (c) (- (char->integer c) 48))) string->list) (aoc-read day)))
(define h (length input))
(define w (length (car input)))

(define (grid-ref xss i j) (list-ref (list-ref xss i) j))

(define (get-neighbours input i j)
  (let ([neighbours (list (cons (sub1 i) j) (cons (add1 i) j) (cons i (sub1 j)) (cons i (add1 j)))])
    (filter-map (match-lambda [(cons i j) (if (or (< i 0) (< j 0) (>= i h) (>= j w)) #f (grid-ref input i j))]) neighbours)))

(define (nal1 input)
  (for*/sum ([i (in-range h)] [j (in-range w)])
    (let ([neighbours (get-neighbours input i j)]
          [height (grid-ref input i j)])
      (if (andmap (cut < height <>) neighbours) (add1 height) 0))))

(aoc-write day 1 (nal1 input))
