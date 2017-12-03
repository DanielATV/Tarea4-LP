#lang scheme

#|
Funcion: collatz
Descripcion: Aplica la conjetura de Collatz a una lista de numeros.
Parametros:
x lista
Retorno: Lista de los mayores valores que se obtienen al aplicar Collatz a cada elemento de la lista.
|#

(define collatz
  (lambda (x)
    #|Metodo auxiliar que va guardando el mayor|#
    (let aux ((val (car x))(leer x)(rsp '())(mayor (car x)))
      #|Si la lista de entrada esta vacia|#
      (if (null? leer)rsp
          (if (eqv? val 1)
              #|Condicion de termino|#
              (if (null? (cdr leer)) (list mayor)
                  #|Avanza en la lista|#
                  (append (list mayor) (aux (car(cdr leer)) (cdr leer) rsp (car(cdr leer)))))
              #|Si es par|#
              (if (even? val)
                  #|Modifica el mayor|#
                  (if (< mayor (/ val 2)) (aux (/ val 2) leer rsp (/ val 2))
                      #|No modifica el mayor|#
                      (aux (/ val 2) leer rsp mayor))
                  #|Si es impar|#
                  (if (< mayor (+ (* 3 val) 1))
                      #|Modifica el mayor|#
                      (aux (+ (* 3 val) 1) leer rsp (+ (* 3 val) 1)) 
                       #|No modifica el mayor|#
                      (aux (+ (* 3 val) 1) leer rsp mayor)
                      )))))))





                      

