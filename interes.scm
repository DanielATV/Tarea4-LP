#lang scheme

#|
Funcion: interes
Descripcion: Aplica la formula de interes compuesto a un capital inicial.
Parametros:
x entero o float
y entero o float
z entero
Retorno: Resultado de aplicar un interes y a un capital x por z meses, mes a mes.
|#

(define interes
  (lambda (x y z)
    (let ((rsp '()))
      #|Se le suma 1 hasta sea igual a la cantidad de meses|#
      (do ((veces 1 (+ veces 1)))
          #|Condicion de termino|#
        ((= veces z) rsp)
          #|Aniade el resultado de la operacion a la lista|#
        (set! rsp (append rsp (list (* x (expt (+ 1 (/ y 100.0)) veces)))))
        ))))



