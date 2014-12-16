#lang racket
;;fold-right
(define(accumulate op in s)
  (if(null? s)
     in
     (op(car s)
        (accumulate op in (cdr s)))))
;;fold-left
(define(fold-left op initial sequence)
  (define (iter result rest )
    (if(null? rest)
       result
       (iter (op result (car rest))
             (cdr rest))))
  (iter initial sequence))