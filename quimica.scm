#lang scheme

(define elementos
  '("escandio" "titanio" "vanadio" "cromo" "magneso" "hierro"
               "cobalto" "niquel" "cobre" "zinc" "itrio" "circonio"
               "niobio" "molibdeno" "tecnecio" "rutenio" "rodio" "paladio"
               "plata" "cadmio" "hafnio" "tantalo" "wolframio" "renio"
               "osmio" "iridio" "platino" "oro" "mercurio" "rutherfordio"
               "dubnio" "seaborgio" "bohrio" "hasio" "meitnerio"
               "darmstatio" "roentgenio" "copernicio"))

#|
Funcion: quimica
Descripcion: Dado un elemento de partida, busca  un elemento en elementos que empieze con la penultima letra del elemento de partida, repite este
proceso con el ultimo que encontro hasta que no encuentre uno distinto a los que lleva.
Parametros:
x: string.
Retorno: Lista con todos los elementos que cumplan la condicion.
|#


(define quimica
  (lambda (x)
    (let aux ((buscar x)(rsp (list x))(elem elementos))
      #|Condicion de termino|#
      (if (null? elem) rsp
          #|Revisa que no se repitan|#
          (if (not (member (car elem) rsp))
              #|Aplica la condicion|#
              (if (eqv? (string-ref buscar (- (string-length buscar) 2))(string-ref (car elem) 0))
                  (aux (car elem) (append rsp (list (car elem))) elementos)
                  #|Si no la cumple sigue con el resto|#
                  (aux buscar rsp (cdr elem))
              )
              #|Si esta ya esta en la lista sigue con el resto|#
              (aux buscar rsp (cdr elem)))))))


