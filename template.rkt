#lang racket

(require (lib "racket/port"))

(define day "00")

(define file-contents
  (string-split
   (port->string (open-input-file (format "day-~a.in" day)) #:close? #t)
   "\n"))

(define reverse
  (lambda (list) (foldl cons null list)))

(define nal1 file-contents)

(define nal2
  (reverse
   (for/list ([l file-contents])
     (string-upcase l))))

(call-with-output-file (format "day-~a-1.out" day)
  #:exists 'truncate
  (lambda (out)
    (display (string-join nal1 "\n") out)))
(call-with-output-file (format "day-~a-2.out" day)
  #:exists 'truncate
  (lambda (out)
    (display (string-join nal2 "\n") out)))
