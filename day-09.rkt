#lang racket

(require "muf-lib.rkt")

(define day "09")

(define input (map (compose (curry map (λ (c) (- (char->integer c) 48))) string->list) (aoc-read day)))
(define h (length input))
(define w (length (car input)))
; (display input)
; (display (~v h))
; (display (~v w))

(define (grid-ref xss i j) (list-ref (list-ref xss i) j))

(define (get-neighbour-heights input i j)
  (let ([neighbours (list (vector (sub1 i) j) (vector (add1 i) j) (vector i (sub1 j)) (vector i (add1 j)))])
    (filter-map (mλ [(vector i j) (if (or (< i 0) (< j 0) (>= i h) (>= j w)) #f (grid-ref input i j))]) neighbours)))

(define (get-low-points input)
  (for*/filter/list ([i (in-range h)] [j (in-range w)])
    (let ([neighbour-heights (get-neighbour-heights input i j)]
          [height (grid-ref input i j)])
      ;   (display-lines (list neighbour-heights height))
      (cond [(andmap (cut < height <>) neighbour-heights) (vector i j)]))))

; (define (get-basin input i j) )
; (displayln (get-low-points input))
; (displayln (get-neighbour-heights input 0 1))
; (displayln (grid-ref input 0 1))
; (displayln (andmap (cut < (grid-ref input 0 1) <>) (get-neighbour-heights input 0 1)))

(define (nal1 input)
  (for/sum/match (in-list (get-low-points input))
    [(vector i j) (add1 (grid-ref input i j))]))

(define (nal2 input) input)

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
