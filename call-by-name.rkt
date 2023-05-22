#lang racket
(require "match.rkt" "env0.rkt" "truep.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (lambda <var> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (random <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | * | =
(define (interp exp env)
  (match exp
    (,int (guard (integer? int)) int)
    (,bool (guard (boolean? bool)) bool)
    (,var (guard (symbol? var)) (force-thunk (apply-env env var)))
    ((if ,e1 ,e2 ,e3)
     (let ((v1 (interp e1 env)))
       (if (true? v1)
           (interp e2 env)
           (interp e3 env))))
    ((lambda ,x ,body)
     (make-closure x body env))
    ((let ,x ,e ,body)
     (let* ((t (make-thunk e env))
            (env^ (extend-env x t env)))
       (interp body env^)))
    ((random ,e)
     (let ((v (interp e env)))
       (random v)))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - * =)))
     (let* ((v1 (interp e1 env))
            (v2 (interp e2 env)))
       (case op
         ((+) (+ v1 v2))
         ((-) (- v1 v2))
         ((*) (* v1 v2))
         ((=) (= v1 v2)))))
    ((,rator ,rand)
     (let* ((closure (interp rator env))
            (arg (make-thunk rand env)))
       (apply-closure closure arg)))))
(define (make-closure formal body env)
  (vector 'closure formal body env))
(define (closure-formal closure)
  (vector-ref closure 1))
(define (closure-body closure)
  (vector-ref closure 2))
(define (closure-env closure)
  (vector-ref closure 3))
(define (apply-closure closure arg)
  (let* ((formal (closure-formal closure))
         (body (closure-body closure))
         (env (closure-env closure)))
    (interp body (extend-env formal arg env))))
(define (make-thunk exp env)
  (vector 'thunk exp env))
(define (thunk-exp thunk)
  (vector-ref thunk 1))
(define (thunk-env thunk)
  (vector-ref thunk 2))
(define (force-thunk thunk)
  (let* ((exp (thunk-exp thunk))
         (env (thunk-env thunk)))
    (interp exp env)))
;> (interp '(let y (lambda f
;                    ((lambda x (f (x x)))
;                     (lambda x (f (x x)))))
;             (let fact (y (lambda fact
;                            (lambda n
;                              (if (= n 0)
;                                  1
;                                  (* n (fact (- n 1)))))))
;               (fact 10)))
;          (empty-env))
;3628800
;> (interp '((lambda n
;              (if (= n 0)
;                  (if (= n 0)
;                      (if (= n 0)
;                          (if (= n 0)
;                              (if (= n 0)
;                                  (if (= n 0)
;                                      (if (= n 0) #t #f)
;                                      #f)
;                                  #f)
;                              #f)
;                          #f)
;                      #f)
;                  (if (= n 0)
;                      #f
;                      (if (= n 0)
;                          #f
;                          (if (= n 0)
;                              #f
;                              (if (= n 0)
;                                  #f
;                                  (if (= n 0)
;                                      #f
;                                      (if (= n 0)
;                                          #f
;                                          #t))))))))
;            (random 2))
;          (empty-env))
;#f
;;the result might be #t