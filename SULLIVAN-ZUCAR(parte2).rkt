;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |SULLIVAN-ZUCAR(parte2)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
                                                ; Sullivan, Katherine
                                                ; Zucar, Francisco
                                                ; Comisión 3

; EJERCICIO 11 (PRÁCTICA 5 SEGUNDA PARTE)
  
; posAuxiliarFoldr : List(Number) -> Number
; Dada una lista de números k, devuelve la suma de sus elementos.

(check-expect (posAuxiliarFoldr (list 1 2 3 4)) 10)
(check-expect (posAuxiliarFoldr empty) 0)

(define (posAuxiliarFoldr k) (foldr + 0 k))

; posAuxiliarMap : List(List(Number)) -> List(Number)
; Dada una lista l de listas de números, devuelve otra lista de números que son el resultado de
; de sumar de los elementos de las listas que son elementos de l.

(check-expect (posAuxiliarMap (list empty (list 1 2 3))) (list 0 6))
(check-expect (posAuxiliarMap (list (list -1 2 -3 4 -5) empty (list -3 -4))) (list -3 0 -7))

(define (posAuxiliarMap l) (map posAuxiliarFoldr l))

; posAuxiliarFilter : List(List(Number)) -> List(Number)
; Dada una lista l de listas de números, devuelve otra lista con los numeros positivos que son el reusltado
; de sumar los elementos de las listas dentro de l.

(check-expect (posAuxiliarFilter (list (list 1 -4 3 0) (list 3 4))) (list 7))
(check-expect (posAuxiliarFilter (list (list -2 0))) empty)

(define (posAuxiliarFilter l) (filter positive? (posAuxiliarMap l)))


; algun-pos : List(List(Number)) -> Bool
; Dada una lista l de listas de números, devuelve #true si para alguna lista de l,
; la suma de sus elementos es positiva.

(check-expect (algun-pos (list (list 1 3 -4 -2) (list 1 2 3 -5) (list 4 -9 -7 8 -3))) #t)
(check-expect (algun-pos (list empty (list 1 2 3))) #t)
(check-expect (algun-pos (list (list -1 2 -3 4 -5) empty (list -3 -4))) #f)

(define (algun-pos l) (cond [(empty? (posAuxiliarFilter l)) #false] [else #true]))