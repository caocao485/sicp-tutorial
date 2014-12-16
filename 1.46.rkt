;;风格一
(define(iterative-improve good-enough? improve)
  (define(try guess)
    (if(good-enough? guess (improve guess))
       guess
       (try (improve guess))))
  try)
;;风格二
(define(iterative-improved improve close-enough?)
  (lambda(x)
    (let((xim(improve x)))
      (if(close-enough? x xim)
         xim
         ((iterative-improved improve close-enough?)xim)))))
;;不动点
(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (improve guess)
        (f guess))
    ((iterative-improve close-enough? improve) first-guess))
;;求平方根
(define (sqrt-1 x)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (improve guess)
        (/(+ guess (/ x guess))2))
    ((iterative-improve close-enough? improve) 1.0))