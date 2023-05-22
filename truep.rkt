#lang racket
(provide true?)
(define (true? val)
  (if (boolean? val)
      val
      (error 'interp "the predicate of if is not of type bool")))