#lang racket
(require "match.rkt" "truep.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  (if <exp> <exp> <exp>)
;       |  (<op> <exp> <exp>)
;<op> ::= + | - | * | =
(define (interp exp)
  (match exp
    (,int (guard (integer? int)) int)
    (,bool (guard (boolean? bool)) bool)
    ((if ,e1 ,e2 ,e3)
     (let ((v1 (interp e1)))
       (if (true? v1)
           (interp e2)
           (interp e3))))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - * =)))
     (let* ((v1 (interp e1))
            (v2 (interp e2)))
       (case op
         ((+) (+ v1 v2))
         ((-) (- v1 v2))
         ((*) (* v1 v2))
         ((=) (= v1 v2)))))))