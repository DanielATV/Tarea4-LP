#lang scheme

#|
Funcion: collatz
Descripcion: Aplica la conjetura de Collatz a una lista de numeros.
Parametros:
n1 entero
n2 entero
Retorno: resultado de la operacion aritmetica de la suma entero
|#

(define collatz
  (lambda (x)
    (let aux ((val (car x))(leer x)(rsp '())(mayor (car x)))
      (if (null? leer)rsp
          (if (eqv? val 1)
              (if (null? (cdr leer)) (list mayor)
                  (append (list mayor) (aux (car(cdr leer)) (cdr leer) rsp (car(cdr leer)))))
              (if (even? val)
                  (if (< mayor (/ val 2)) (aux (/ val 2) leer rsp (/ val 2))
                      (aux (/ val 2) leer rsp mayor))
                  (if (< mayor (+ (* 3 val) 1))(aux (+ (* 3 val) 1) leer rsp (+ (* 3 val) 1))
                      (aux (+ (* 3 val) 1) leer rsp mayor)
                      )))))))





                      

