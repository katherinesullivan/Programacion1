;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pel) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Practica 6 all over again
; copiar: Natural String -> List(String)
; El propósito de la función copiar es crear una lista de
; n copias de una cadena s
;(check-expect (copiar 2 "hola") (list "hola" "hola"))
;(check-expect (copiar 0 "hola") '())
;(check-expect (copiar 4 "abc") (list "abc" "abc" "abc" "abc"))

(define (copiar n s) (cond [(zero? n) '()]
                           [(positive? n) (cons s (copiar (sub1 n) s))]))

; sumanat: Natural Natural -> Natural
; suma dos números sin usar el operador +
;(check-expect (sumanat 2 3) 5)
;(check-expect (sumanat 0 6) 6)
;(check-expect (sumanat 0 0) 0)

(define
  (sumanat x y)
  (cond [(zero? x) y]
        [(zero? y) x]
        [else (add1 (sumanat x (sub1 y)))]))

; Sigma: Natural (Natural -> Number) -> Number
; suma todos los valores naturales del 0 al n que toma una función dada
;(check-expect (Sigma 4 sqr) 30)

(define
  (Sigma n f)
  (if (zero? n) (f 0) (sumanat (f n) (Sigma (- n 1) f))))



