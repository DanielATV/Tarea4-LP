#lang scheme

#|
Funcion: codificar
Descripcion: Cambia los 1 por True y los 0 por False.
Parametros:
x entero
Retorno: True si recibe un 1 y False si recibe un 0.
|#
(define codificar
  (lambda (x)
    (if (eqv? x 1)#t
        #f)))
#|
Funcion: decodificar
Descripcion: Cambia los True por 1 y los False por 0.
Parametros:
x boleano
Retorno: 1 si recibe True y 0 si recibe False.
|#

(define decodificar
  (lambda (x)
    (if (eqv? x #t)1
        0)))

#|
Funcion: operar
Descripcion: Toma el i-esimo de ambas listas y se le aplica un operador.
Parametros:
x funcion
y lista
z lista
Retorno: Lista de resultados de aplicar el operador a cada i-esimo elemento de ambas listas.
|#

(define operar
  (lambda (x)
    (lambda (y z)
      (let ((rsp '()))
        #|Condicion de termino|#
        (if (null?  y) rsp
            #|Aniade el elemento luego de aplicar el operador x|#
            (append rsp (list (decodificar (x (codificar (car y)) (codificar (car z))))) ((operar x) (cdr y) (cdr z)))
            )))))

