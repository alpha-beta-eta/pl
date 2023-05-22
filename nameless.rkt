#lang racket
(require "match.rkt" "truep.rkt")
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
(define (interp exp denv)
  (match exp
    (,int (guard (integer? int)) int)
    (,bool (guard (boolean? bool)) bool)
    ((var ,depth) (apply-denv denv depth))
    ((if ,e1 ,e2 ,e3)
     (let ((v1 (interp e1 denv)))
       (if (true? v1)
           (interp e2 denv)
           (interp e3 denv))))
    ((lambda ,body)
     (make-closure body denv))
    ((let ,e ,body)
     (let* ((v (interp e denv))
            (denv^ (extend-denv v denv)))
       (interp body denv^)))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - * =)))
     (let* ((v1 (interp e1 denv))
            (v2 (interp e2 denv)))
       (case op
         ((+) (+ v1 v2))
         ((-) (- v1 v2))
         ((*) (* v1 v2))
         ((=) (= v1 v2)))))
    ((,rator ,rand)
     (let* ((closure (interp rator denv))
            (arg (interp rand denv)))
       (apply-closure closure arg)))))
(define (empty-denv) '())
(define (extend-denv val denv)
  (cons val denv))
(define (apply-denv denv depth)
  (list-ref denv depth))
(define (make-closure body denv) (vector 'closure body denv))
(define (closure-body closure) (vector-ref closure 1))
(define (closure-denv closure) (vector-ref closure 2))
(define (apply-closure closure arg)
  (let* ((body (closure-body closure))
         (denv (closure-denv closure)))
    (interp body (extend-denv arg denv))))
;> (interp
;   '(let (lambda
;           (lambda
;             (if (= (var 0) 0)
;                 1
;                 (* (var 0) (((var 1) (var 1)) (- (var 0) 1))))))
;      (let ((var 0) (var 0))
;        ((var 0) 10)))
;   (empty-denv))
;3628800