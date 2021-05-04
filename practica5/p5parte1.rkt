;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname p5parte1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|Ejercicio 3 . Diseñe las listas de valores booleanos. Este tipo de dato debe permitir
listas de cualquier longitud.|#

; ListaBool es:
; - una lista vacia '() o
; - una expresión del tipo (cons bool ListaBool)
; donde bool puede tomar el valor #true o #false

#|Operador     Tipo de Operador          Función
'()            Constructor               Representa la lista vacía
empty?         Predicado                 Reconoce únicamente la lista vacía
cons           Constructor               Agrega un elemento a una lista
first          Selector                  Devuelve el primer elemento de la lista
rest           Selector                  Devuelve la lista sin su primer elemento
cons?          Predicado                 Reconoce listas no vacías|#

(equal? (list 0 1 2 3 4 5) (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(equal? (list (list 1) (list 2) (list 2 3)) (cons (cons 1 '()) (cons (cons 2 '()) (cons 2 (cons 3 '()))))) #false
(equal? (list 2 (list 3 1) 4) '(2 (3 1) 4))


#|Es importante notar que list no es un constructor, sino un operador
que nos provee el lenguaje para simplificar la escritura de las listas en nuestros
programas.|#

; Ejercicio 4

; contiene-Marcos? : Contactos -> Booleano
; dada una lista de Contactos, determina si "Marcos" es un elemento de la misma
(check-expect (contiene-Marcos? '()) #false)
(check-expect (contiene-Marcos? (cons "Sara" (cons "Pedro"
(cons "Esteban" '())))) #false)
(check-expect (contiene-Marcos? (cons "A" (cons "Marcos" (cons "C" '())))) #true)
(check-expect (contiene-Marcos? (cons "Juan" '())) #false)
(check-expect (contiene-Marcos? (cons "Marcos" '())) #true)

(define
  (contiene-Marcos? l)
  (cond [(empty? l) #false]
        [(cons? l) (if (string=? (first l) "Marcos")
                       #true
                       (contiene-Marcos? (rest l)))]))

(contiene-Marcos? (cons "Eugenia"
                        (cons "Lucía"
                              (cons "Dante"
                                    (cons "Federico"
                                          (cons "Marcos"
                                                (cons "Gabina"
                                                      (cons "Laura"
                                                            (cons "Pamela" '())))))))))

; Ejercicio 5
; DrRacket provee una función member? que toma un valor xy una lista l y chequea si el
; valor x está en la lista l.

; contiene?: String List(String) -> Bool
; Determina si una Lista de Strings contiene un determinado string

(check-expect (contiene? "" '()) #false)
(check-expect (contiene? "Hola" (cons "hola" '())) #false)
(check-expect (contiene? "hola" (list "hola" "chau")) #true)

(define
  (contiene? str l)
  (cond [(empty? l) #false]
        [(cons? l) (if (string=? (first l) str)
             #true
             (contiene? str (rest l)))]
        [else "Tipo de dato incorrecto"]))

; Ejercicio 6
(contiene-Marcos? (cons "Marcos" (cons "C" '())))

; Ejercicio 7
;Una Lista-de-montos es:
;– '()
;– (cons NumeroPositivo Lista-de-montos)
;Lista-de-montos representa una lista con montos de dinero

(check-expect (suma (list 1 2 3 4)) 10)
(check-expect (suma '()) 0)
(check-expect (suma 2) "Tipo de dato incorrecto")

(define
  (suma l)
  (cond [(empty? l) 0]
        [(cons? l) (+ (first l) (suma (rest l)))]
        [else "Tipo de dato incorrecto"]
        ))

; Ejercicio 8

; Lista-de-numeros -> Bool

(define
  (pos? l)
  (cond [(empty? l) #true]
        [(cons? l) (if (< (first l) 0)
                       #false
                       (pos? (rest l)))]))

; Lista-de-numeros -> Number | String
(define
  (checked-suma l)
  (if (pos? l)
      (suma l)
      "La lista no es una lista de montos"))

; Ejercicio 9


(check-expect (todos-verdaderos (list #t #t #f)) #f)
(check-expect (todos-verdaderos '()) #t)
(check-expect (todos-verdaderos (list #f #f)) #f)
; Lista(Bool) -> Bool
(define
  (todos-verdaderos l)
  (cond [(empty? l) #true]
        [(cons? l) (if (equal? (first l) #false)
                       #false
                       (todos-verdaderos (rest l)))]
        ))

(check-expect (uno-verdadero (list #t #t #t)) #t)
(check-expect (uno-verdadero '()) #f)
(check-expect (uno-verdadero (list #f #f)) #f)
; Lista(Bool) -> Bool
(define
  (uno-verdadero l)
  (cond [(empty? l) #false]
        [(cons? l) (if (equal? (first l) #true)
                       #true
                       (uno-verdadero (rest l)))]))

; Ejercicio 13

#|Diseñe una función cortas que tome una lista de strings y devuelva una
lista con aquellas palabras de longitud menor a 5.|#

; longitud: List -> Number
; longitud toma una lista y devuelve su cantidad de elementos

#|(check-expect (longitud '()) 0)
(check-expect (longitud (list 1 2 3 4)) 4)
(check-expect (longitud (list "hola")) 1)
(check-expect (longitud (list (list 1) (list "a"))) 2)
(check-expect (longitud (list '())) 1)|#

(define
  (longitud l)
  (cond [(empty? l) 0]
        [(cons? l) (add1 (longitud (rest l)))]
        [else "Error de tipo"]
        ))


; Representanos palabras con strings
; corta? : String -> Bool
; corta? toma una string y devuelve #true si su longitud es menor a 5 y false si no es así
(check-expect (corta? "") #t)
(check-expect (corta? "hola") #t)
(check-expect (corta? "corta") #f)

(define
  (corta? s)
  (< (string-length s) 5)
  )

; Representamos palabras con strings
; cortas: List(String) -> List(String)
; La función cortas toma una lista de strings y nos devuelve
; otra con aquellas strings con longitud menor a 5

(check-expect (cortas (list "soy" "corta" "o" "no" "holiss")) (list "soy" "o" "no") )
(check-expect (cortas '()) '())
(check-expect (cortas (list "somos" "todas" "palabras" "largas")) '())
(check-expect (cortas (list "so" "mos" "to" "das" "cor" "tas")) (list "so" "mos" "to" "das" "cor" "tas"))

(define
  (cortas l)
  (cond [(empty? l) '()]
        [(cons? l)
         (if (corta? (first l))
             (cons (first l) (cortas (rest l)))
             (cortas (rest l)))
         ]
        )
  )




; Ejercicio 17

(check-expect (eliminar (list 1 2 3 2 7 6) 2) (list 1 3 7 6))
(check-expect (eliminar (list 1 2 3 2 7 6) 0) (list 1 2 3 2 7 6))
(check-expect (eliminar '() 0) '())

; List(Number) Number -> List(Number)
(define
  (eliminar l n)
  (cond [(empty? l) l]
        [(cons? l) (if (= (first l) n)
                       (eliminar (rest l) n)
                       (cons (first l) (eliminar (rest l) n))
                       )]
        ))