#lang scheme

(define codificar
  (lambda (x)
    (if (eqv? x 1)#t
        #f)))

(define decodificar
  (lambda (x)
    (if (eqv? x #t)1
        0)))

(define operar
  (lambda (x)
    (lambda (y z)
      (let ((rsp '()))
        (if (null?  y) rsp
            (append rsp (list (decodificar (x (codificar (car y)) (codificar (car z))))) ((operar x) (cdr y) (cdr z)))
            )))))

