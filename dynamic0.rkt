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
     (make-lambda x body))
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
     (let* ((lam (interp rator env))
            (arg (interp rand env)))
       (apply-lambda lam arg env)))))
(define (make-lambda formal body)
  (lambda (arg env)
    (interp body (extend-env formal arg env))))
(define (apply-lambda lam arg env)
  (lam arg env))
;> (interp
;   '(let fact (lambda n
;                (if (= n 0)
;                    1
;                    (* n (fact (- n 1)))))
;      (fact 10))
;   (empty-env))
;3628800
;> (interp '(let two 2
;             (let double (lambda x (* two x))
;               (let two 3
;                 (double 4))))
;          (empty-env))
;12