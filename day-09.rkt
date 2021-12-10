#lang racket

(require "muf-lib.rkt")

(define day "09")

(define input (map (compose (curry map (λ (c) (- (char->integer c) 48))) string->list) (aoc-read day)))
(define h (length input))
(define w (length (car input)))

(define/match (grid-ref xss v) [(xss (vector i j)) (list-ref (list-ref xss i) j)])

(define/match (get-neighbours input v)
  [(input (vector i j))
   (let ([neighbours (list (vector (sub1 i) j) (vector (add1 i) j) (vector i (sub1 j)) (vector i (add1 j)))])
     (filter-map (mλ [(vector i j) (if (or (< i 0) (< j 0) (>= i h) (>= j w)) #f (vector i j))]) neighbours))])

(define (get-neighbour-heights input v)
  (map (cut grid-ref input <>) (get-neighbours input v)))

(define (get-low-points input)
  (for*/filter/list ([i (in-range h)] [j (in-range w)] [v (in-value (vector i j))])
    (let ([neighbour-heights (get-neighbour-heights input v)]
          [height (grid-ref input v)])
      (cond [(andmap (cut < height <>) neighbour-heights) v]))))

(define (get-basin input v)
  (let* ([height (grid-ref input v)]
         [neighbours (get-neighbours input v)]
         [neighbours (filter (compose (λ (n) (and (< height n) (< n 9))) (cut grid-ref input <>)) neighbours)])
    (remove-duplicates (flatten (cons (map (cut get-basin input <>) neighbours) v)))))

(define (nal1 input)
  (for/sum ([v (in-list (get-low-points input))])
    (add1 (grid-ref input v))))

(define (nal2 input)
  (apply * (take (sort (map (compose length (cut get-basin input <>)) (get-low-points input)) >) 3)))

(aoc-write day 1 (nal1 input))
(aoc-write day 2 (nal2 input))
