#lang planet neil/sicp
;;prime n
(define (divides? a b)
  (= (remainder b a)0))
(define (square x)(* x x))
(define (find-divisor n test-divisor)
  (cond((>(square test-divisor) n)n)
       ((divides? test-divisor n)test-divisor)
       (else(find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define(prime? n)
  (= n (smallest-divisor n)))

;;display
(define (report-prime elsapsed-time)
  (display "***")
  (display elsapsed-time))
(define (start-prime-test n  start-time)
  (if(prime? n)
     (report-prime (-(runtime) start-time))))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

;;search
(define(search-for-primes from n)
  (cond((= n 0)(newline) 'done)
       ((even? from)(search-for-primes (+ from 1) n))
       ((timed-prime-test from)(search-for-primes (+ from 2)(- n 1)))
       (else (search-for-primes (+ from 2) n))))
