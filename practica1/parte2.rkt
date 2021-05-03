;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parte2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define
  (sgn2 x)
  (cond [(< x 0) -1]
        [(= x 0) 0]
        [(> x 0) 1]))

#|(sgn2 (- 2 3))
(sgn2 6)


(define
  (pitagorica? a b c)
  (cond [(= (+ (sqr a) (sqr b)) (sqr c)) #true]
        [(= (+ (sqr a) (sqr c)) (sqr b)) #true]
        [(= (+ (sqr b) (sqr c)) (sqr a)) #true]
        [ #true #false]))

(pitagorica? 3 5 4)|#

(require 2htdp/image)
#|Hemos decidido hilar más fino en la clasificación de
imágenes. Ahora diremos que una imagen es "Muy gorda" si su ancho
es más que el doble que su alto. Del mismo modo, diremos que "Muy
flaca" si su alto es más que el doble que su ancho. Defina una función,
utilizando una expresión cond , que clasifique imágenes en alguna de
las categorías "Muy Gorda" , "Gorda" , "Cuadrada" , "Flaca" , "Muy
flaca" .|#

(define (clasif-imagen img)
  (cond [(= (image-width img) (image-height img)) "Cuadrada"]
        [(> (image-height img) (* 2 (image-width img))) "Muy flaca"]
        [(< (image-width img) (image-height img)) "Flaca"]
        [(> (image-width img) (* 2 (image-height img))) "Muy gorda"]
        [(> (image-width img) (image-height img)) "Gorda"]))

(image-width (bitmap "/home/administrador/Imágenes/belu.png"))
(* 2 (image-height (bitmap "/home/administrador/Imágenes/belu.png")))
(clasif-imagen (bitmap "/home/administrador/Imágenes/belu.png"))
(clasif-imagen (bitmap "/home/administrador/Imágenes/atormentado.jpeg"))

(define
  (clasificar t)
  (cond [(<= t 0) "No Olvidar Bufanda (NOB)"]
        [(and (> t 0) (<= t 15)) "Frío (F)"]
        [(and (> t 15) (<= t 25)) "Agradable (A)"]
        [(> t 25) "Realmente Caluroso (RC)"]))

(clasificar 15)
(clasificar 25)
(clasificar -3)

(define
  (bool->number bool)
  (if (equal? bool #true) 1
      (if (equal? bool #false) 0 "Error"))) ; antes de llamar esta se fija que sea un booleano entonces no haría falta

(define
  (sgn3 x)
  (cond [(number? x) (sgn2 x)]
        [(string? x) (sgn2 (string->number x))]
        [(boolean? x) (sgn2 (bool->number x))]))

(sgn3 #false)

(define
  (bool->number2 bool)
  (if (equal? bool #true) 1 0))

(define
  (image->clasifnumber img)
  (cond [(equal? (clasif-imagen img) "Flaca") -1]
        [(string=? (clasif-imagen img) "Muy flaca") -1]
        [(string=? (clasif-imagen img) "Cuadrada") 0]
        [(equal? (clasif-imagen img) "Gorda") 1]
        [(string=? (clasif-imagen img) "Muy gorda") 1]))

(define
  (sgn4 x)
  (cond [(number? x) (sgn2 x)]
        [(string? x) (sgn2 (string->number x))]
        [(boolean? x) (sgn2 (bool->number2 x))]
        [(image? x) (sgn2 (image->clasifnumber x))]
        [else "Tipo no soportado por la función"]))

(sgn4 (bitmap "/home/administrador/Imágenes/belu.png"))

(define
  (sgn5 x)
  (cond [(number? x) (sgn2 x)]
        [(string? x) (let ([n (string->number x)]) (if (number? n) (sgn2 n) "La cadena no se puede convertir a número"))]
        [(boolean? x) (sgn2 (bool->number2 x))]
        [(image? x) (sgn2 (image->clasifnumber x))]
        [else "Tipo no soportado por la función"]))

(sgn5 "Hola")
