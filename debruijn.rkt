#lang racket
(require "match.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (lambda <var> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | * | =
(define (compil exp senv)
  (match exp
    (,int (guard (integer? int)) int)
    (,bool (guard (boolean? bool)) bool)
    (,var
     (guard (symbol? var))
     (let ((depth (apply-senv senv var)))
       `(var ,depth)))
    ((if ,e1 ,e2 ,e3)
     (let* ((e1^ (compil e1 senv))
            (e2^ (compil e2 senv))
            (e3^ (compil e3 senv)))
       `(if ,e1^ ,e2^ ,e3^)))
    ((lambda ,x ,body)
     (let ((body^ (compil body (extend-senv x senv))))
       `(lambda ,body^)))
    ((let ,x ,e ,body)
     (let* ((e^ (compil e senv))
            (senv^ (extend-senv x senv))
            (body^ (compil body senv^)))
       `(let ,e^ ,body^)))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - * =)))
     (let* ((e1^ (compil e1 senv))
            (e2^ (compil e2 senv)))
       `(,op ,e1^ ,e2^)))
    ((,rator ,rand)
     (let* ((rator^ (compil rator senv))
            (rand^ (compil rand senv)))
       `(,rator^ ,rand^)))))
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (lambda <exp>)
;       |  (let <exp> <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<var> ::= (var <int>)
;<op> ::= + | - | * | =
(define (empty-senv) '())
(define (extend-senv var senv)
  (cons var senv))
(define (apply-senv senv var)
  (let iter ((rest senv) (depth 0))
    (cond ((null? rest)
           (error 'apply-senv "unbound variable ~s" var))
          ((eq? (car rest) var) depth)
          (else (iter (cdr rest) (+ depth 1))))))
;> (compil
;   '(let make-fact (lambda make-fact
;                     (lambda n
;                       (if (= n 0)
;                           1
;                           (* n ((make-fact make-fact) (- n 1))))))
;      (let fact (make-fact make-fact)
;        (fact 10)))
;   (empty-senv))
;'(let (lambda (lambda (if (= (var 0) 0) 1 (* (var 0) (((var 1) (var 1)) (- (var 0) 1))))))
;   (let ((var 0) (var 0)) ((var 0) 10)))