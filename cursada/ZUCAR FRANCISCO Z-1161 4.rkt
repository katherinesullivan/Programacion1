;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |ZUCAR FRANCISCO Z-1161 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Jugando al solitario
; Francisco Zucar
; Legajo: Z-1161/4

; Práctica 7

-(define N 40) ; N es la cantidad de cartas

; cartas: Natural -> List(Natural)
; dado un número n, devuelve una lista con los números ordenados en forma creciente del 1 al n
(check-expect (cartas 0) empty)
(check-expect (cartas 10) (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (cartas 4) (list 1 2 3 4))

(define
  (cartas n)
  (cond [(= 0 n) empty]
        [else (reverse (cons n (reverse (cartas (- n 1)))))]))

(define CARTAS (cartas N)); cartas con las que se juega

; abarajar: List -> List
; Dada una lista, la devuelve con sus elementos desordenados.
(define
  (abarajar l)
  (let ([cartita (list-ref l (random (length l)))])
        (cond [(empty? (rest l)) l]
              [else (cons cartita
                    (abarajar (remove cartita l)))])))

; el-jueguito: List(Number) List(Number) -> String
; dadas dos listas con los mismos elementos, devuelve #false si dos elementos iguales están en la misma posición y #true si no.
(check-expect (el-jueguito (list 1 2 3 4) (list 2 3 4 1)) #t)
(check-expect (el-jueguito (list 1 2 3 4 5 6 7 8 9 10)(list 4 3 2 1 6 7 8 6 9 10)) #f)
(check-expect (el-jueguito (list 1 2 3 4 5 6) (list 1 2 3 4 5 6)) #f)

(define
  (el-jueguito lista listita)
  (cond [(empty? lista) #t]
        [(= (first lista) (first listita)) #f]
        [else (el-jueguito (rest lista) (rest listita))]))

; jugar: Number -> List
; dado un número n, devuelve una lista con la función "el-jueguito" hecha n veces con la lista CARTAS y la lista que devuelve el hacer "abarajar" con CARTAS.

(define
  (jugar n)
  (cond [(= 0 n) empty]
        [else (cons (el-jueguito CARTAS (abarajar CARTAS))
                     (jugar (- n 1)))]))


(define TIRADAS 7000)


(define juego-mucho (jugar TIRADAS))

(define (true? x) (not (false? x)))

(define resultado-con-Montecarlo (/ (length (filter true? juego-mucho)) (length juego-mucho)))

(string-append "Rocío debe apostar, ya que Lautaro tiene solo "  (number->string (exact->inexact resultado-con-Montecarlo)) " chances de ganar")