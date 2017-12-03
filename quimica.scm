#lang scheme

(define elementos
  '("escandio" "titanio" "vanadio" "cromo" "magneso" "hierro"
               "cobalto" "niquel" "cobre" "zinc" "itrio" "circonio"
               "niobio" "molibdeno" "tecnecio" "rutenio" "rodio" "paladio"
               "plata" "cadmio" "hafnio" "tantalo" "wolframio" "renio"
               "osmio" "iridio" "platino" "oro" "mercurio" "rutherfordio"
               "dubnio" "seaborgio" "bohrio" "hasio" "meitnerio"
               "darmstatio" "roentgenio" "copernicio"))

(define quimica
  (lambda (x)
    (let aux ((buscar x)(rsp (list x))(elem elementos))
      (if (null? elem) rsp
          (if (not (member (car elem) rsp))
              (if (eqv? (string-ref buscar (- (string-length buscar) 2))(string-ref (car elem) 0))
                  (aux (car elem) (append rsp (list (car elem))) elementos)
                  (aux buscar rsp (cdr elem))
              )
              (aux buscar rsp (cdr elem)))))))

(quimica "molibdeno")
(quimica "oro")
              
