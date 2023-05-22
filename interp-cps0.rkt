#lang racket
(require "match.rkt" "env0.rkt")
;<exp> ::= <var>
;       |  (lambda <var> <exp>)
;       |  (<exp> <exp>)
(define (interp exp env k)
  (match exp
    (,var
     (guard (symbol? var))
     (k (apply-env env var)))
    ((lambda ,x ,body)
     (k (lambda (arg k)
          (interp body (extend-env x arg env) k))))
    ((,rator ,rand)
     (interp rator env
             (lambda (closure)
               (interp rand env
                       (lambda (arg)
                         (closure arg k))))))))