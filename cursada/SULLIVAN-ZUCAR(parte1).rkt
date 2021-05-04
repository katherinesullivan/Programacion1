;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |SULLIVAN-ZUCAR(parte1)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
                                                   ; Sullivan, Katherine
                                                   ; Zucar, Francisco
                                                   ; Comisión 3

; EJERCICIO 14 (PRÁCTICA 5 PRIMERA PARTE) 

; Mayores: list number -> list
; Evaluando una lista dada devuelve una nueva que contenga
; los elementos de la lista original mayores a un num dado

(define
  (mayores l n)
  (cond [(empty? l) empty]
        [else (if (> (first l) n) (cons (first l) (mayores (rest l) n))
              (mayores (rest l) n))]))