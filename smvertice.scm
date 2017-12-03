#lang racket

(define formato
  (lambda (x)
    (let aux ((resp '()) (llevo x))
      (if (null? llevo) resp
          (aux (append resp (car llevo))(cdr llevo))))))

(define buscar
  (lambda (x y)
    (let aux ((buscar x) (falta y))
      (if (null? falta) '()
      (if (= buscar (car(car falta))) (formato (cdr(car falta)))
          (aux x (cdr falta)))))))

(define sinrepe
  (lambda (x y)
    (let aux ((llevo x) (falta y))
      (if (null? falta) llevo
          (if (not(member (car falta) llevo)) (aux (append llevo (list (car falta))) (cdr falta))
              (aux llevo (cdr falta)))))))

(define completar
  (lambda (x y z)
    (let aux((llevo x) (falta y))
      (if (null? falta) llevo
          (aux (append (sinrepe llevo (buscar (car falta) z))) (cdr falta))))))

(define smvertice
  (lambda (x)
    (let aux ((actual (car(car x))) (visitados '()) (faltan (cdr x)) (resp '()) (vecinos (append (list (car(car x)))(formato(cdr(car x))))))
      (if (null? faltan)
          (if (= (length visitados) 0) (aux actual '(1) faltan resp (append (list actual)(completar vecinos vecinos x)))
              (if (= (length vecinos) (length x))(append resp (list actual))
                  resp))
          (if (= (length vecinos) (length x)) (aux (car(car faltan)) '() (cdr faltan) (append resp (list actual)) (formato(cdr(car faltan))))
              (if (= (length visitados) 0) (aux actual '(1) faltan resp (append (list actual)(completar vecinos vecinos x)))
                  (aux (car(car faltan)) '() (cdr faltan) resp (formato(cdr(car faltan))))))))))


(smvertice '((1 (2))
(2 (1 5))
(3 (2))
(4 (6))
(5 (6))
(6 (1))
(7 (2 3 4))))
