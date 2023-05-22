#lang racket
(provide (all-defined-out))
(define (empty-env)
  (lambda (var)
    (error 'apply-env "unbound variable ~s" var)))
(define (extend-env var val env)
  (lambda (var^)
    (if (eq? var var^)
        val
        (apply-env env var^))))
(define (apply-env env var)
  (env var))