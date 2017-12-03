#lang scheme
#|
Funcion: Suma_Enteros
Descripcion: suma dos enteros positivos
Parametros:
n1 entero
n2 entero
Retorno: resultado de la operacion aritmetica de la suma entero
|#

(define codificar
  (lambda (x)
    (if (eqv? x 1)#t
        #f)))
#|
Funcion: Suma_Enteros
Descripcion: suma dos enteros positivos
Parametros:
n1 entero
n2 entero
Retorno: resultado de la operacion aritmetica de la suma entero
|#

(define decodificar
  (lambda (x)
    (if (eqv? x #t)1
        0)))

#|
Funcion: Suma_Enteros
Descripcion: suma dos enteros positivos
Parametros:
n1 entero
n2 entero
Retorno: resultado de la operacion aritmetica de la suma entero
|#

(define operar
  (lambda (x)
    (lambda (y z)
      (let ((rsp '()))
        (if (null?  y) rsp
            (append rsp (list (decodificar (x (codificar (car y)) (codificar (car z))))) ((operar x) (cdr y) (cdr z)))
            )))))

