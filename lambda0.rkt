#lang racket
(require "match.rkt" "env0.rkt" "truep.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (lambda <var> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | * | =
(define (interp exp env)
  (match exp
    (,int (guard (integer? int)) int)
    (,bool (guard (boolean? bool)) bool)
    (,var (guard (symbol? var)) (apply-env env var))
    ((if ,e1 ,e2 ,e3)
     (let ((v1 (interp e1 env)))
       (if (true? v1)
           (interp e2 env)
           (interp e3 env))))
    ((lambda ,x ,body)
     (lambda (arg)
       (interp body (extend-env x arg env))))
    ((let ,x ,e ,body)
     (let* ((v (interp e env))
            (env^ (extend-env x v env)))
       (interp body env^)))
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
            (arg (interp rand env)))
       (closure arg)))))
;> (interp
;   '(let make-fact (lambda make-fact
;                     (lambda n
;                       (if (= n 0)
;                           1
;                           (* n ((make-fact make-fact) (- n 1))))))
;      (let fact (make-fact make-fact)
;        (fact 10)))
;   (empty-env))
;3628800
;> (interp
;   '(let y (lambda h
;             ((lambda f (f f))
;              (lambda g
;                (h (lambda x ((g g) x))))))
;      (let fact (y (lambda fact
;                     (lambda n
;                       (if (= n 0)
;                           1
;                           (* n (fact (- n 1)))))))
;        (fact 10)))
;   (empty-env))
;3628800