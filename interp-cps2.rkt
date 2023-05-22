#lang racket
(require "match.rkt" "env0.rkt" "truep.rkt")
;<exp> ::= <int>
;       |  <bool>
;       |  <var>
;       |  (if <exp> <exp> <exp>)
;       |  (lambda <var> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (letrec <var> (lambda <var> <exp>) <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | * | =
(define EXP (void))
(define ENV (void))
(define VAL (void))
(define CLO (void))
(define CTX (void))
(define depth (void))
(define max-depth (void))
(define (PUSH x)
  (set! CTX (cons x CTX))
  (set! depth (+ depth 1))
  (when (> depth max-depth)
    (set! max-depth depth)))
(define (POP)
  (if (null? CTX)
      (error 'POP "can't pop an empty stack")
      (let ((x (car CTX)))
        (set! CTX (cdr CTX))
        (set! depth (- depth 1))
        x)))
(define (initialize! exp)
  (set! EXP exp)
  (set! ENV (empty-env))
  (set! VAL (void))
  (set! CLO (void))
  (set! CTX '())
  (set! depth 0)
  (set! max-depth 0))
(define (INTERP)
;  (when (equal? EXP '(fact (- n 1)))
;    (printf "~s\n" CTX))
  (match EXP
    (,int
     (guard (integer? int))
     (set! VAL int)
     (APPLY_CTX))
    (,bool
     (guard (boolean? bool))
     (set! VAL bool)
     (APPLY_CTX))
    (,var
     (guard (symbol? var))
     (set! VAL (apply-env ENV var))
     (APPLY_CTX))
    ((if ,e1 ,e2 ,e3)
     (set! EXP e1)
     (PUSH (if-ctx e2 e3 ENV))
     (INTERP))
    ((lambda ,x ,body)
     (set! VAL (make-closure x body ENV))
     (APPLY_CTX))
    ((let ,x ,e ,body)
     (set! EXP e)
     (PUSH (let-ctx x body ENV)))
    ((letrec ,f (lambda ,x ,fbody) ,body)
     (set! EXP body)
     (set! ENV (extend-env-rec f x fbody ENV))
     (INTERP))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - * =)))
     (set! EXP e1)
     (PUSH (op-ctx-1 op e2 ENV))
     (INTERP))
    ((,rator ,rand)
     (set! EXP rator)
     (PUSH (rator-ctx rand ENV))
     (INTERP))))
(define (APPLY_CTX)
  (unless (null? CTX)
    (let ((top (POP)))
      (match top
        ((if-ctx ,e2 ,e3 ,env)
         (if (true? VAL)
             (set! EXP e2)
             (set! EXP e3))
         (set! ENV env)
         (INTERP))
        ((let-ctx ,x ,body ,env)
         (set! EXP body)
         (set! ENV (extend-env x VAL env))
         (INTERP))
        ((op-ctx-1 ,op ,e2 ,env)
         (set! EXP e2)
         (set! ENV env)
         (PUSH (op-ctx-2 op VAL))
         (INTERP))
        ((op-ctx-2 ,op ,v1)
         (case op
           ((+) (set! VAL (+ v1 VAL)))
           ((-) (set! VAL (- v1 VAL)))
           ((*) (set! VAL (* v1 VAL)))
           ((=) (set! VAL (= v1 VAL))))
         (APPLY_CTX))
        ((rator-ctx ,rand ,env)
         (set! EXP rand)
         (set! ENV env)
         (PUSH (rand-ctx VAL))
         (INTERP))
        ((rand-ctx ,closure)
         (set! CLO closure)
         (APPLY_CLO))))))
(define (APPLY_CLO)
  (set! EXP (closure-formal CLO))
  (set! ENV (closure-env CLO))
  (set! ENV (extend-env EXP VAL ENV))
  (set! EXP (closure-body CLO))
  (INTERP))
(define (make-closure formal body env)
  (vector 'closure formal body env))
(define (closure-formal closure)
  (vector-ref closure 1))
(define (closure-body closure)
  (vector-ref closure 2))
(define (closure-env closure)
  (vector-ref closure 3))
(define (set-closure-env! closure env)
  (vector-set! closure 3 env))
(define (extend-env-rec f x fbody env)
  (let* ((closure (make-closure x fbody 'foo))
         (env^ (extend-env f closure env)))
    (set-closure-env! closure env^)
    env^))
(define (if-ctx e2 e3 env)
  `(if-ctx ,e2 ,e3 ,env))
(define (let-ctx x body env)
  `(let-ctx ,x ,body ,env))
(define (op-ctx-1 op e2 env)
  `(op-ctx-1 ,op ,e2 ,env))
(define (op-ctx-2 op v1)
  `(op-ctx-2 ,op ,v1))
(define (rator-ctx rand env)
  `(rator-ctx ,rand ,env))
(define (rand-ctx closure)
  `(rand-ctx ,closure))
;> (initialize! '(letrec fact (lambda n
;                               (if (= n 0)
;                                   1
;                                   (* n (fact (- n 1)))))
;                  (fact 10)))
;> (INTERP)
;> VAL
;3628800
;> max-depth
;12
;> (initialize! '(letrec fact (lambda n
;                               (if (= n 0)
;                                   1
;                                   (* n (fact (- n 1)))))
;                  (fact 20)))
;> (INTERP)
;> VAL
;2432902008176640000
;> max-depth
;22
;> (initialize! '(letrec fact (lambda n
;                               (if (= n 0)
;                                   1
;                                   (* n (fact (- n 1)))))
;                  (fact 30)))
;> (INTERP)
;> VAL
;265252859812191058636308480000000
;> max-depth
;32
;> (initialize! '(letrec < (lambda a
;                            (lambda b
;                              (if (= b 0)
;                                  #f
;                                  (if (= a 0)
;                                      #t
;                                      ((< (- a 1)) (- b 1))))))
;                  ((< 9) 10)))
;> (INTERP)
;> VAL
;#t
;> max-depth
;3
;> (initialize! '(letrec < (lambda a
;                            (lambda b
;                              (if (= b 0)
;                                  #f
;                                  (if (= a 0)
;                                      #t
;                                      ((< (- a 1)) (- b 1))))))
;                  ((< 90) 100)))
;> (INTERP)
;> VAL
;#t
;> max-depth
;3
;> (initialize! '(letrec < (lambda a
;                            (lambda b
;                              (if (= b 0)
;                                  #f
;                                  (if (= a 0)
;                                      #t
;                                      ((< (- a 1)) (- b 1))))))
;                  ((< 900) 1000)))
;> (INTERP)
;> VAL
;#t
;> max-depth
;3
;> (initialize! '(letrec fact (lambda n
;                               (if (= n 0)
;                                   1
;                                   (* n (fact (- n 1)))))
;                  (fact 5)))

;> (INTERP)
;((op-ctx-2 * 5))
;((op-ctx-2 * 4) (op-ctx-2 * 5))
;((op-ctx-2 * 3) (op-ctx-2 * 4) (op-ctx-2 * 5))
;((op-ctx-2 * 2) (op-ctx-2 * 3) (op-ctx-2 * 4) (op-ctx-2 * 5))
;((op-ctx-2 * 1) (op-ctx-2 * 2) (op-ctx-2 * 3) (op-ctx-2 * 4) (op-ctx-2 * 5))