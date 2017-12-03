#lang scheme


(define interes
  (lambda (x y z)
    (let ((rsp '()))
      (do ((veces 1 (+ veces 1)))
        ((= veces z) rsp)
        (set! rsp (append rsp (list (* x (expt (+ 1 (/ y 100.0)) veces)))))
        ))))



