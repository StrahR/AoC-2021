#lang racket

(require (lib "racket/port"))

(define day "01")

(define file-contents
  (map string->number
       (string-split
        (port->string (open-input-file (format "day-~a.in" day)) #:close? #t)
        "\n")))

(define (reverse list)
  (foldl cons null list))

(define (nal1 ll)
  (define (aux acc xs)
    (match xs
      [(or '() (list _)) acc]
      [(cons y (cons x xs)) #:when (< y x) (aux (add1 acc) (cons x xs))]
      [(cons _ xs) (aux acc xs)]
      ))
  (aux 0 ll))

(call-with-output-file (format "day-~a-1.out" day)
  #:exists 'truncate
  (lambda (out)
    (display (~a (nal1 file-contents)) out)))
