#lang scheme

#|
Funcion: raiz
Descripcion: Aproxima el resultado de una raiz cuadrada.
Parametros:
x entero
y entero
Retorno: Resultado de aplicar la aproximacion a x y veces.
#||#


(define raiz
  (lambda (x y)
    #|Metedo auxiliar que va guardando el resultado|#
    (let aux((num x)(ite 0) (llevo (/ x 2.0)))
      (if (= y ite) llevo
          (aux num (+ ite 1) (* 0.5 (+ llevo (/ num llevo))))))))
