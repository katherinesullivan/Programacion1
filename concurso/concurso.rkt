;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname concurso) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ejercicio 13 - Práctica 5, parte 1

#|Diseñe una función cortas que tome una lista de strings y devuelva una
lista con aquellas _palabras_ de longitud menor a 5.|#

; corta? : String -> Bool
; dado un string me va a devolver #true si este tiene longitud menor a 5 y el string consta de una sola
; palabra y #false en caso contrario

(check-expect (pal-corta? "palabra") #f)
(check-expect (pal-corta? "hey") #t)
(check-expect (pal-corta? "") #t)
(check-expect (pal-corta? "a a") #f)

(define
  (pal-corta? s)
  (and (< (string-length s) 5) (false? (member? #\space (string->list s))))
  )

; aclarar que los que tengan mas de una palabra no va


; cortas: List(String) -> List(String)
; dada una lista de strings devuelve una lista con aquellas palabras con longitud menor a 5

(check-expect (cortas '()) '())
(check-expect (cortas (list "holas" "queeee" "tallll")) '())
(check-expect (cortas (list "hola" "antiparras" "comunismo")) (list "hola"))
(check-expect (cortas (list "hola" "que" "tal")) (list "hola" "que" "tal"))
(check-expect (cortas 5) "Tipo de dato ingresado inválido")
(check-expect (cortas (list "oh f" "rayo" "australia")) (list "rayo"))

(define
  (cortas l)
  (cond [(empty? l) '()]
        [(cons? l) (if (pal-corta? (first l))
                   (cons (first l) (cortas (rest l)))
                   (cortas (rest l)))]
        [else "Tipo de dato ingresado inválido"]
        )
  )