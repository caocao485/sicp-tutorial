#lang Scheme
;;nil
(define nil '())
;;归约（累积）
(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) (accumulate op initial (cdr sequence)))))
;;枚举
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
;;映射+append做累积
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;;定义empty-board
(define empty-board nil)
;;添加皇后，（只用添加 new-row即可），方向是逆序的。
(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
;;过滤不安全皇后
(define(safe? k positions)
  (define(safe-1iter? k1 list i)
    (if (null? list)
        #t
        (and(not(= k1 (- (car list) i)))
            (not(= k1 (+ (car list) i)))
            (not(= k1 (car list)))
            (safe-1iter? k1 (cdr list) (+ i 1)))))
  (safe-1iter? (car positions) (cdr positions) 1))
         
;;queens主函数
(define(queens board-size)
  (define (queen-cols k)
    (if(= k 0)
       (list empty-board)
       (filter 
        (lambda(positions)(safe? k positions))
        (flatmap
         (lambda(rest-of-queens)
           (map (lambda(new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queen-cols (- k 1))))))
  (queen-cols board-size))

;计数
(define(count n)
  (length(queens n)))