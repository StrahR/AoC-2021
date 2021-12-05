#lang racket

(require "muf-lib.rkt")
(require megaparsack megaparsack/text data/applicative data/monad)

(define day "05")

(define day05/p
  (do [x1 <- integer/p]
    (char/p #\,)
    [y1 <- integer/p]
    (string/p " -> ")
    [x2 <- integer/p]
    (char/p #\,)
    [y2 <- integer/p]
    (pure (list x1 y1 x2 y2))))

(define input (map (λ (s) (parse-result! (parse-string day05/p s))) (aoc-read day)))
; (display input)

(define (add-line vents a start end dir)
  (if (> start end) vents
      (match dir
        ['x (add-line (hash-update vents (cons a start) add1 0) a (add1 start) end dir)]
        ['y (add-line (hash-update vents (cons start a) add1 0) a (add1 start) end dir)])))

(define (nal1 input)
  (count (cut > <> 1)
         (hash-values
          (for/fold/match (in-list input)
            ([vents (hash)])
            [(list x1 y1 x2 y2) #:when (= y1 y2) (add-line vents y1 (min x1 x2) (max x1 x2) 'x)]
            [(list x1 y1 x2 y2) #:when (= x1 x2) (add-line vents x1 (min y1 y2) (max y1 y2) 'y)]
            [_ vents]))))

(define (nal2 input) input)

; (define vents (nal1 input))
; (for* ([x (in-range 10)] [y (in-range 10)])
; (display (hash-ref vents (cons x y) ".")))

(aoc-write day 1 (nal1 input))
; (aoc-write day 2 (nal2 input))
