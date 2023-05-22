#lang racket
(provide make-store)
(require "match.rkt")
(define (make-store size)
  (define store (make-vector size))
  (define next-loc 0)
  (define (newref val)
    (if (= next-loc size)
        (error 'newref "no space for allocation")
        (let ((loc next-loc))
          (vector-set! store loc val)
          (set! next-loc (+ next-loc 1))
          `(ref ,loc))))
  (define (deref ref)
    (let ((loc (match ref
                 ((ref ,loc) loc)
                 (,else
                  (error 'deref
                         "~s is not a reference."
                         ref)))))
      (vector-ref store loc)))
  (define (setref ref val)
    (let ((loc (match ref
                 ((ref ,loc) loc)
                 (,else
                  (error 'deref
                         "~s is not a reference."
                         ref)))))
      (vector-set! store loc val)))
  (lambda (msg . arg*)
    (let ((proc (case msg
                  ((newref) newref)
                  ((deref) deref)
                  ((setref) setref))))
      (apply proc arg*))))