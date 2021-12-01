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

(define (nal2 ll)
  (define (aux acc xs)
    (match* (xs (cdr xs) (cddr xs) (cdddr xs))
      [(_ _ _ '()) acc]
      [((cons x xs) (cons y _) (cons z _) (cons w _))
       #:when (< (+ x y z) (+ y z w))
       (aux (add1 acc) xs)]
      [((cons _ xs) _ _ _) (aux acc xs)]
      ))
  (aux 0 ll))

(call-with-output-file (format "day-~a-1.out" day)
  #:exists 'truncate
  (lambda (out)
    (display (~a (nal1 file-contents)) out)))

(call-with-output-file (format "day-~a-2.out" day)
  #:exists 'truncate
  (lambda (out)
    (display (~a (nal2 file-contents)) out)))
