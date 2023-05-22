#lang racket
(provide (all-defined-out))
(define (empty-env) '())
(define (extend-env var val env)
  (cons (cons var val) env))
(define (apply-env env var)
  (cond ((assq var env) => cdr)
        (else (error 'apply-env "unbound variable ~s" var))))