#lang scheme

#|
Funcion: raiz
Descripcion: Aproxima el resultado de una raiz cuadrada.
Parametros:
x entero
y entero
Retorno: Resultado de aplicar la aproximacion y veces.
|#


(define raiz
  (lambda (x y)
    (let aux((num x)(ite 0) (llevo (/ x 2.0)))
      (if (= y ite) llevo
          (aux num (+ ite 1) (* 0.5 (+ llevo (/ num llevo))))))))
