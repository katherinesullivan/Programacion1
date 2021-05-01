;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Katherine Sullivan-Jugando al Solitario|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Katherine Sullivan
;Legajo: S-5436/4
;Jugando al solitario
;Práctica 7: parte 2

(define N 40) ; N representa la cantidad de cartas en el mazo

;Primero que nada tendremos que crear el mazo.
;Este será una lista con los números del 1 al n ordenados en forma creciente.

; lista-n: Natural -> List(Natural)
; dado un número n, devuelve una lista con los números del 1 al n
(check-expect (lista-n 1) (list 1))
(check-expect (lista-n 0) empty)
(check-expect (lista-n 8) (list 1 2 3 4 5 6 7 8))
(check-expect (lista-n 3) (list 1 2 3))

(define
  (lista-n n)
  (cond [(zero? n) empty]
        [else (reverse (cons n (reverse (lista-n (sub1 n)))))]))

;Con esta función entonces podremos definir el mazo
(define MAZO (lista-n N))

;Ahora necesitamos simular lo que sería mezclar las cartas y lo haremos con una
;función que mezcle los elementos de una lista.

; mezclar-cartas: List -> List
; Dada una lista, devuelve otra con la misma longitud y los mismos elementos,
; pero con la posición de los mismos dada de manera aleatoria.

(define
  (mezclar-cartas l)
  (let* ([z (list-ref l (random (length l)))])
        (cond [(empty? (rest l)) l]
              [else (cons z
                    (mezclar-cartas (remove z l)))])))

;Teniendo todos los elementos, ahora podemos diseñar nada-en-su-lugar

; nada-en-su-lugar: List(Number) List(Number) -> String
; dadas dos listas que contienen los mismos números y la misma
; cantidad de ellos, si dos números que son iguales coinciden en
; posición, la función devolverá "partida perdida", en caso contrario,
; devolverá "partida ganada"
(check-expect (nada-en-su-lugar
               (list 1 2 3 4)
               (list 4 3 2 1)) "partida ganada")
(check-expect (nada-en-su-lugar
               (list 1 2 3 4 5 6 7 8 9)
               (list 4 3 2 1 5 7 8 9 6)) "partida perdida")
(check-expect (nada-en-su-lugar
               empty empty) "partida ganada")
(check-expect (nada-en-su-lugar
               (list 1 2 3 4 5 6 7 8 9)
               (list 4 3 2 1 6 7 8 6 9)) "partida perdida")
(check-expect (nada-en-su-lugar
               (list 1 2 3 4 5 6)
               (list 1 2 4 3 5 6)) "partida perdida")

(define
  (nada-en-su-lugar l1 l2)
  (cond [(empty? l1) "partida ganada"]
        [(= (first l1) (first l2)) "partida perdida"]
        [else (nada-en-su-lugar (rest l1) (rest l2))]))

       
;Ahora construiremos una función que simule el juego de Francisco una cierta
;cantidad de veces.

; simular-n-veces: Number List List -> List
; dado un número n, devuelve una lista cuyos elementos son los resultados de 
; hacer nada-en-su-lugar con la lista MAZO y la lista que resulta de aplicar
; mezclar-mazo a MAZO

(define
  (simular-n-veces n)
  (cond [(zero? n) empty]
        [else (cons (nada-en-su-lugar MAZO (mezclar-cartas MAZO))
                     (simular-n-veces (sub1 n)))]))

;Definiremos una constante que determine las veces que el programa jugará
;"nada en su lugar"

(define VECES-JUGADAS 20000)

;El programa que simula a Francisco jugando una cantidad fija de veces el
;"nada en su lugar", que nos servirá para el cálculo que Rocío quiere hacer,
;es así:

(define simulación (simular-n-veces VECES-JUGADAS))

;Ahora necesitamos algo para clasificar las partidas ganadas de las perdidas.

; partida-ganada?: String -> Bool
; dado un string devuelve #true si es "partida ganada" y #false en caso
; contrario
(check-expect (partida-ganada? "partida perdida") #false)
(check-expect (partida-ganada? "partida ganada") #true)
(check-expect (partida-ganada? "partida ganada.") #false)
(check-expect (partida-ganada? "hola") #false)

(define
  (partida-ganada? x)
  (if (string=? x "partida ganada") #t #f))

;Entonces, el método de Montecarlo aplicado a este problema quedaría así:

(define Montecarlo (/ (length (filter partida-ganada? simulación))
                      (length simulación)))

; Y la respuesta entonces es: 

(string-append "La posibilidad de que Francisco gane es de aproximadamente "
               (number->string (exact->inexact Montecarlo))
               ". El que Rocío apueste dependerá de lo arriesgada que sea, "
               "pero, en términos generales, le convendría apostarle a Lautaro")