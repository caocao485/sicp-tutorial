#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 以下三个定义 initial-env, ent-env, lookup 是对环境(environment)的基本操作:

;;初始环境 
(define initial-env (list (cons '+ +)(cons '- -)(cons '* *)(cons '/ /)(cons '> >)(cons '< <)(cons '= =)
                    (cons 'cons cons)))

;; 扩展。对环境 env 进行扩展,把 x 映射到 v,得到一个新的环境   
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

;; 查找。在环境中 env 中查找 x 的值    
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) (error x"undefined;
cannot reference an identifier before its definition\n" )]  
       [else (cdr p)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 闭包的数据结构定义，包含一个函数定义 f 和它定义时所在的环境      
(struct Closure (f env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 解释器的递归定义（接受两个参数，表达式 exp 和环境 env）      
;; 共 12 种情况（变量，函数，布尔,调用，if,数字，算术表达式）
(define eval1
  (lambda (exp env)
     (match exp                                            ;;模式匹配 exp 的以下情况（分支）                         
       [(? string? exp) exp]                               ;;字符串                           
       [(? number? exp)  exp]                              ;;数字
       [(? boolean? exp) exp]                              ;;布尔
       [(? symbol? exp) (lookup exp env)]                  ;;符号
       [`(lambda (,x) ,e)                                  ;; 函数
       (Closure exp env)]
       ;;;修改ing
       [`(cond . ,e  )                                     ;;cond
       (let ((e1 (first e))
             (re (rest e)))
        [match e1
          [`(else ,e2)
           (eval1 e2 env)]
          [else (if(eval1 (first e1) env)                 
                  (eval1 (second e1) env)
                  (eval1  (cons `cond re) env))]])]
      [`(and . ,exps )                                     ;;and
          (cond((empty?  exps)#t)
               ((not(eval1 (first exps) env))#f)
               (else(eval1 (cons 'and (rest exps)) env)))]
      [`(or . ,exps )                                      ;;or
          (cond((empty?  exps)#f)
               ((eval1 (first exps) env)#t)
               (else(eval1 (cons 'and (rest exps)) env)))]
      [`(begin  ,exps ...)                                 ;;begin
       (foldl (lambda (e _) (eval1 e env)) empty exps)]                             
      [`(set! ,var ,exps)                                  ;;set!
       (let((x1(assq var env)))(set-cdr! x1 (eval  exps env)))]
      [`(define ,name ,exps)                               ;; define(有小问题）
       (let ([value (eval1 exps env)])
         (set! initial-env (ext-env name value env)))]
       ;;;修改ing
      [`(if ,ec ,et ,ef)    (if (eval1 ec env)             ;;if表达式
                              (eval1 et env)
                              (eval1 ef env))]
       [`(quote ,e)                                        ;;quote
       e]
       [`(,op . ,e1 )                                      ;;调用
       (let ([op1 (eval1 op env)]
             [v1  (eval-map e1 env)])
         (match op1
                [(Closure `(lambda (,x) ,e) env1)
                 (eval1 e (ext-env x v1 env1))]
                [else
                 (apply op1 v1)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;一些辅佐函数
(define (eval-map e1 env)
  (if(= (length e1)1)
     (eval1 (car e1) env)
     (map (lambda(e)(eval1 e env)) e1)))
(define first car)
(define second cadr)
(define rest cdr)
(define-syntax-rule (set-car! lst val)                     ;;添加set-car!
(if (not (pair? lst))
     lst
     (set! lst (cons val (cdr lst))))) 
(define-syntax-rule (set-cdr! lst val)                     ;;添加set-car!
  (if (not (list? lst))
     lst
  (set! lst (cons(car lst) val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 解释器的“用户界面”函数和启动函数,初始值为 initial-env    
(define welcome 
       (lambda()
          (display "\n")
          (display "**********************************\n")
          (display "*                                *\n")
          (display "*     The Jinscheme REPL         *\n")
          (display "*       By jinsheng cao          *\n")
          (display "*                                *\n")
          (display "**********************************\n")
          (display "\n") ))
(define start
  (lambda ()
    (call/cc 
     (lambda (k)
       (display ">>>")
       (let ([exp (read)])
         (if (not (equal? '退出 exp))
             (display (eval1 exp initial-env))
             (k 'done)))
       (newline)
     (start)))))
(define (jinscheme)
     (welcome)
    (start))
(jinscheme)