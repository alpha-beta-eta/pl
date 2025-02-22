#lang racket
(require "match.rkt" "env0.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (lambda (<var> : <type>) <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (<exp> <exp>)
;<type> ::= int
;        |  bool
;        |  (-> <type> <type>)
(define (type-of exp env)
  (match exp
    (,int (guard (integer? int)) 'int)
    (,bool (guard (boolean? bool)) 'bool)
    (,var (guard (symbol? var)) (apply-env env var))
    ((if ,q ,a ,e)
     (let ((tq (type-of q env)))
       (unless (equal? tq 'bool)
         (error 'type-of "the predicate of if should be a bool"))
       (let* ((ta (type-of a env))
              (te (type-of e env)))
         (unless (equal? ta te)
           (error 'type-of
                  "the consequent and alternative of if should be of the same type"))
         ta)))
    ((lambda (,var : ,type) ,body)
     (let ((t (type-of body (extend-env var type env))))
       `(-> ,type ,t)))
    ((let ,x ,e ,body)
     (let ((t (type-of e env)))
       (type-of body (extend-env x t env))))
    ((,rator ,rand)
     (let* ((t1 (type-of rator env))
            (t2 (type-of rand env)))
       (match t1
         ((-> ,t3 ,t4)
          (unless (equal? t3 t2)
            (error 'type-of "~s expected, ~s given" t3 t2))
          t4)
         (,else
          (error 'type-of "rator should be a function")))))))
(define prim-env
  (extend-env
   '+ '(-> int (-> int int))
   (extend-env
    'zero? '(-> int bool)
    (extend-env
     '< '(-> int (-> int bool))
     (empty-env)))))
