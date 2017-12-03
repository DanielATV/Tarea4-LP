#lang racket

#|
Funcion: formato
Descripcion: Saca los elementos dentro de una lista de listas.
Parametros:
x lista
Retorno: Lista con los elementos del interior de la lista de listas x.
|#

(define formato
  (lambda (x)
    (let aux ((resp '()) (llevo x))
      #|Condicion de termino|#
      (if (null? llevo) resp
          #|Avanza en la lista|#
          (aux (append resp (car llevo))(cdr llevo))))))
#|
Funcion: buscar
Descripcion: Encuentra los nodos adyacentes de un nodo.
Parametros:
x entero
y lista
Retorno: Lista de los nodos adyacentes del vertice x en el grafo y.
|#

(define buscar
  (lambda (x y)
    (let aux ((buscar x) (falta y))
      #|Condicion de termino|#
      (if (null? falta) '()
          #|Si lo encuentra|#
      (if (= buscar (car(car falta))) (formato (cdr(car falta)))
          #|Avanza en la lista|#
          (aux x (cdr falta)))))))
#|
Funcion: sinrepe
Descripcion: Crea una lista de elementos no repetidos.
Parametros:
x lista
y lista
Retorno: Concatenacion de la lista x e y sin elementos repetidos.
|#
(define sinrepe
  (lambda (x y)
    (let aux ((llevo x) (falta y))
      #|Condicion de termino|#
      (if (null? falta) llevo
          #|Aniade el elemento|#
          (if (not(member (car falta) llevo)) (aux (append llevo (list (car falta))) (cdr falta))
              #|Avanza en la lista si esta repetido|#
              (aux llevo (cdr falta)))))))

#|
Funcion: completar
Descripcion: Aniade a una lista  de vecinos los vecidos de cada elemento de otra lista.
Parametros:
x lista
y lista
z lista
Retorno: Lista de vecinos de x mas los vecinos de cada elemento de y, sin repeteir, en el grafo z.
|#
(define completar
  (lambda (x y z)
    (let aux((llevo x) (falta y))
      #|Condicion de termino|#
      (if (null? falta) llevo
          #|Aniade sin repetir|#
          (aux (append (sinrepe llevo (buscar (car falta) z))) (cdr falta))))))

#|
Funcion: smvertice
Descripcion: Encuentra los nodos semi-madre.
Parametros:
x lista
Retorno: Lista con todos los nodos semi-madre del grafo x.
|#

(define smvertice
  (lambda (x)
    #|Metodo auxiliar que verifica si los todos los vecinos han sido considerados|#
    (let aux ((actual (car(car x))) (visitados '()) (faltan (cdr x)) (resp '()) (vecinos (formato(cdr(car x)))))
      (if (null? faltan)
          #|Si  hay vecinos sin considerar|#
          (if (= (length visitados) 0) (aux actual '(1) faltan resp (completar vecinos vecinos x))
              #|Si el ultimo nodo de la lista es semi madre|#
              (if (>= (length vecinos) (length x))(append resp (list actual))
                  #|Termino|#
                  resp))
          #|Si alcanza a todos los nodos sin pasar por nodos intermedios|#
          (if (>= (length vecinos) (length x)) (aux (car(car faltan)) '() (cdr faltan) (append resp (list actual)) (formato(cdr(car faltan))))
              #|Si  hay vecinos sin considerar|#
              (if (= (length visitados) 0) (aux actual '(1) faltan resp (completar vecinos vecinos x))
                  #|Si ya considero todos los vecinos  pero no es semi madre|#
                  (aux (car(car faltan)) '() (cdr faltan) resp (formato(cdr(car faltan))))))))))
