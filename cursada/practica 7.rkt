;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |practica 7|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define CARAS 6)
; intervalo: n
(define
  (intervalo n)
  (cond [(zero? n) empty]
        [else (cons n (intervalo (sub1 n)))]))

; simular-dado: Natural -> List(Number)
; dado un numero n devuelve una lista con n números aleatorios entre 1 y CARAS
;(check-expect 

(define
  (simular-dado n)
  (cond [(= n 0) empty]
        [else (cons (+ (random CARAS) 1) (simular-dado (- n 1)))]))

(define MAX 60000)
(define EXPERIMENTO (simular-dado MAX))
(define VALORES (intervalo CARAS))

; frecuencia: Natural List(Natural) -> Natural
; dado un número natural n y una lista de naturales,
; devuelve la cantidad de veces que n aparece en la lista

(define
  (frecuencia n l)
  (if (empty? l) 0
      (if (= (first l) n) (+ 1 (frecuencia n (rest l)))
           (frecuencia n (rest l)))))
;(frecuencia 1 (list 1 1 1 2))

; frecuencia-relativa: Natural List -> Natural
; dada una lista de números y un natural n, devuelve la división entre la
; cantidad de veces que aparece n en la lista y la longitud de la lista,
; es decir, la frecuencia relativa de n en la lista

(define (frecuencia-relativa n l) (/  (frecuencia n l) (length l)))

; frec-rel-exp: Natural -> Natural
; dado un natural, n, calcula la frecuencia relativa en la que n aparece en
; experimento

(define (frec-rel-exp n) (frecuencia-relativa n EXPERIMENTO))
; (frec-rel-exp 1)

(define FRECUENCIAS-RELATIVAS (map frec-rel-exp VALORES))
; FRECUENCIAS-RELATIVAS

(define
  (simular-dos-dados n)


