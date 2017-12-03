#lang scheme


(define raiz
  (lambda (x y)
    (let aux((num x)(ite 0) (llevo (/ x 2.0)))
      (if (= y ite) llevo
          (aux num (+ ite 1) (* 0.5 (+ llevo (/ num llevo))))))))
