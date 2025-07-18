#lang racket
(require "match.rkt" "env0.rkt" "store.rkt" "truep.rkt")
(define store0 (make-store 10000))
(define (newref val)
  (store0 'newref val))
(define (deref ref)
  (store0 'deref ref))
(define (setref ref val)
  (store0 'setref ref val))
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (set! <var> <exp>)
;       |  (if <exp> <exp> <exp>)
;       |  (lambda <var> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (begin <exp>+)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | * | =
(define (interp exp env)
  (match exp
    (,int (guard (integer? int)) int)
    (,bool (guard (boolean? bool)) bool)
    (,var (guard (symbol? var)) (deref (apply-env env var)))
    ((set! ,x ,e)
     (guard (symbol? x))
     (let* ((v (interp e env))
            (r (apply-env env x)))
       (setref r v)))
    ((if ,e1 ,e2 ,e3)
     (let ((v1 (interp e1 env)))
       (if (true? v1)
           (interp e2 env)
           (interp e3 env))))
    ((lambda ,x ,body)
     (make-closure x body env))
    ((let ,x ,e ,body)
     (let* ((v (interp e env))
            (r (newref v))
            (env^ (extend-env x r env)))
       (interp body env^)))
    ((begin . ,e+)
     (interp-e+ e+ env))
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
            (arg (interp rand env))
            (ref (newref arg)))
       (apply-closure closure ref)))))
(define (interp-e+ e+ env)
  (if (null? e+)
      (error 'interp-e+ "the body of a begin should not be empty")
      (let iter ((e (car e+)) (e* (cdr e+)))
        (if (null? e*)
            (interp e env)
            (begin (interp e env)
                   (iter (car e*) (cdr e*)))))))
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
;> (interp '(let fact 0
;             (begin (set! fact
;                          (lambda n
;                            (if (= n 0)
;                                1
;                                (* n (fact (- n 1))))))
;                    (fact 10)))
;          (empty-env))
;3628800
;> (interp '(let counter (let x 0
;                          (lambda _
;                            (begin (set! x (+ x 1))
;                                   x)))
;             (begin (counter 0)
;                    (counter 0)
;                    (counter 0)))
;          (empty-env))
;3
;> (interp
;   '(let swap (lambda a
;                (lambda b
;                  (let t a
;                    (begin
;                      (set! a b)
;                      (set! b t)))))
;      (let x 0
;        (let y 1
;          (begin
;            ((swap x) y)
;            x))))
;   (empty-env))
;0