#lang racket

(require math/array)
(require "muf-lib.rkt")

(define day "04")

(define-values (input bingos)
  (let* ([in (file->string (format "day-~a.in" day))]
         [in (string-split in "\n\n")]
         [in (map (λ (s) (string-replace s "\n" " ")) in)])
    (values (map string->number
                 (string-split (car in) ","))
            (map (λ (bingo)
                   (list->array #(5 5) (map string->number (string-split bingo #:repeat? #t))))
                 (cdr in)))))

; (display (~v input))
; (display (~v bingos))

(define (has-winning-row? bingo)
  (array-ormap (λ (s) (< s 0)) (array-axis-max bingo 1)))
(define (has-winning-col? bingo)
  (array-ormap (λ (s) (< s 0)) (array-axis-max bingo 0)))
; (< (array-axis-max bingo 1) 0))

; (let ([arr (list->array #(3 2) '(-1 -2 1 -2 3 -4))])
;   (display arr)
;   (display (array-axis-max arr 1))
;   (format "row: ~a; col: ~a"
;           (has-winning-row? arr)
;           (has-winning-col? arr)))

(define (is-winning? bingo) (or (has-winning-row? bingo) (has-winning-col? bingo)))

(define (winning bingos)
  (car (filter is-winning? bingos)))

; (define (mark k bingo)
;   (array-map (λ (n) (if (= n k) (- n) n)) bingo))
(define (mark k)
  (λ (bingo) (array-map (λ (n) (if (= n k) (- -1 n) n)) bingo)))

(define (score bingo)
  (array-all-sum (array-map (λ (n) (if (< n 0) 0 n)) bingo)))

(define (nal1 input bingos)
  (for/fold ([bingos bingos]
             [n 0]
             #:result (* n (score (winning bingos))))
            ([k (in-list input)])
    #:break (ormap is-winning? bingos)
    (values (map (mark k) bingos) k)))

(define (nal2 input bingos) "")

(aoc-write day 1 (nal1 input bingos) #t)
(aoc-write day 2 (nal2 input bingos))
