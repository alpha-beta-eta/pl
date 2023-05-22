#lang racket
(require "match.rkt" "env0.rkt" "store.rkt")
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
;       |  (cons <exp> <exp>)
;       |  (car <exp>)
;       |  (cdr <exp>)
;       |  (set-car! <exp> <exp>)
;       |  (set-cdr! <exp> <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | * | =
(define (interp exp env)
  (match exp
    (,int (guard (integer? int)) `(int ,int))
    (,bool (guard (boolean? bool)) `(bool ,bool))
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
    ((cons ,e1 ,e2)
     (let* ((a (interp e1 env))
            (d (interp e2 env))
            (ra (newref a))
            (rd (newref d))
            (la (match ra
                  ((ref ,loc) loc))))
       `(pair ,la)))
    ((car ,e)
     (let ((v (interp e env)))
       (match v
         ((pair ,la) (deref `(ref ,la)))
         (,else (error 'interp:car "~s is not a pair" v)))))
    ((cdr ,e)
     (let ((v (interp e env)))
       (match v
         ((pair ,la) (let ((ld (+ la 1)))
                       (deref `(ref ,ld))))
         (,else (error 'interp:cdr "~s is not a pair" v)))))
    ((set-car! ,e1 ,e2)
     (let* ((v1 (interp e1 env))
            (v2 (interp e2 env))
            (la (match v1
                  ((pair ,la) la)
                  (,else
                   (error 'interp:set-car! "~s is not a pair" v1))))
            (ra `(ref ,la)))
       (setref ra v2)))
    ((set-cdr! ,e1 ,e2)
     (let* ((v1 (interp e1 env))
            (v2 (interp e2 env))
            (la (match v1
                  ((pair ,la) la)
                  (,else
                   (error 'interp:set-car! "~s is not a pair" v1))))
            (ld (+ la 1))
            (rd `(ref ,ld)))
       (setref rd v2)))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - * =)))
     (let* ((v1 (interp e1 env))
            (v2 (interp e2 env))
            (i1 (match v1
                  ((int ,i) i)
                  (,else (error 'interp:op:v1 "~s is not an integer" v1))))
            (i2 (match v2
                  ((int ,i) i)
                  (,else (error 'interp:op:v2 "~s is not an integer" v2)))))
       (case op
         ((+) `(int ,(+ i1 i2)))
         ((-) `(int ,(- i1 i2)))
         ((*) `(int ,(* i1 i2)))
         ((=) `(bool ,(= i1 i2))))))
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
(define (true? val)
  (match val
    ((bool ,b) b)
    (,else (error 'interp "the predicate of if is not of type bool"))))
(define (value-of obj)
  (match obj
    ((int ,i) i)
    ((bool ,b) b)
    ((pair ,la)
     (let* ((ld (+ la 1))
            (ra `(ref ,la))
            (rd `(ref ,ld))
            (oa (deref ra))
            (od (deref rd)))
       (cons (value-of oa) (value-of od))))
    (,closure closure)))
;> (value-of
;   (interp
;    '(let < 0
;       (let quotient 0
;         (let remainder 0
;           (let ext-gcd 0
;             (begin
;               (set! < (lambda a
;                         (lambda b
;                           (if (= b 0)
;                               #f
;                               (if (= a 0)
;                                   #t
;                                   ((< (- a 1)) (- b 1)))))))
;               (set! quotient
;                     (lambda a
;                       (lambda b
;                         (if ((< a) b)
;                             0
;                             (+ ((quotient (- a b)) b) 1)))))
;               (set! remainder
;                     (lambda a
;                       (lambda b
;                         (if ((< a) b)
;                             a
;                             ((remainder (- a b)) b)))))
;               (set! ext-gcd
;                     (lambda a
;                       (lambda b
;                         (if (= b 0)
;                             (cons 1 0)
;                             (let q ((quotient a) b)
;                               (let r ((remainder a) b)
;                                 (let p ((ext-gcd b) r)
;                                   (let m (car p)
;                                     (let n (cdr p)
;                                       (cons n (- m (* q n))))))))))))
;               ((ext-gcd 123) 321))))))
;    (empty-env)))
;'(47 . -18)
;> (value-of
;   (interp
;    '(let p (cons 0 1)
;       (let q (cons p p)
;         (begin (set-car! p (+ (car p) 2))
;                (set-cdr! p (+ (cdr p) 3))
;                q)))
;    (empty-env)))
;'((2 . 4) 2 . 4)