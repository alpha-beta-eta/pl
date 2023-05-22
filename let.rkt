#lang racket
(require "match.rkt" "env0.rkt" "truep.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (<op> <exp> <exp>)
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
         ((=) (= v1 v2)))))))
;> (interp '(let x 0
;             (let x (+ x 1)
;               (let x (+ x 2)
;                 (+ x 3))))
;          (empty-env))
;6