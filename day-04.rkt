#lang racket

(require math/array)
(require srfi/26)
(require "muf-lib.rkt")

(define day "04")

(define (string->bingo bingo)
  (list->array #(5 5) (map string->number (string-split bingo #:repeat? #t))))

(define-values (input bingos)
  (let* ([in (file->string (format "day-~a.in" day))]
         [in (string-split in "\n\n")])
    (values (map string->number (string-split (car in) ","))
            (map string->bingo (cdr in)))))

(define (is-winning? bingo)
  (define (has-winning-row? bingo) (array-ormap (λ (s) (< s 0)) (array-axis-max bingo 1)))
  (define (has-winning-col? bingo) (array-ormap (λ (s) (< s 0)) (array-axis-max bingo 0)))
  (or (has-winning-row? bingo) (has-winning-col? bingo)))

(define (score bingo)
  (array-all-sum (array-map (λ (n) (if (< n 0) 0 n)) bingo)))

(define (mark k bingo)
  (if (number? bingo) bingo
      (let ([new-bingo (array-map (λ (n) (if (= n k) (- -1 n) n)) bingo)])
        (if (is-winning? new-bingo)
            (* k (score new-bingo))
            new-bingo))))

(define (nal-loop input bingos brcond)
  (for/fold ([bingos bingos]
             #:result (car (filter number? bingos)))
            ([k (in-list input)])
    #:break (brcond bingos)
    (map (cut mark k <>) (filter-not number? bingos))))

(define (nal1 input bingos) (nal-loop input bingos (cut ormap  number? <>)))
(define (nal2 input bingos) (nal-loop input bingos (cut andmap number? <>)))

(aoc-write day 1 (nal1 input bingos))
(aoc-write day 2 (nal2 input bingos))
