;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |pract parcial|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Raíces: List (Number) -> List (Number)
; dada una lista de números, devuelve una lista con las raíces cuadradas
; de los elementos de la primera.
(check-expect (raíces (list 9 16 4)) (list 3 4 2))
(check-expect (raíces '()) '())


(define
  (raíces l)
  (cond [(empty? l) empty]
        [else (cons (sqrt (first l)) (raíces (rest l)))]
        ))

(check-expect (map sqrt (list 9 4 16)) (list 3 2 4)) ;!!!!!!!!!!!!!!!!!!!!!!!!

(map sqrt (list 9 16 4))

; Distancias: List (posn) -> List (Number)
; dada una lista de puntos en el plano, la función devuelve una lista
; con la distancia al origen de cada punto de la lista original.

(check-expect (distancias (list (make-posn 3 4) (make-posn 0 4)
                                (make-posn 0 4) (make-posn 12 5)))
              (list 5 4 4 13))
(check-expect (distancias (list (make-posn 0 0))) (list 0))
(check-expect (distancias '()) '())

(define
  (distancias l)
  (cond [(empty? l) empty]
        [else (cons (sqrt (+ (sqr (posn-x (first l)))
                       (sqr (posn-y (first l)))))
                    (distancias (rest l)))]
        ))

; dist-or: Posn -> Number
; Dado un punto en el plano (expresado en una estructura posn,
; devuelve un número que es la ditancia al origen del punto.
(check-expect (dist-or (make-posn 3 4)) 5)
(check-expect (dist-or (make-posn 0 4)) 4)
(check-expect (dist-or (make-posn 12 5)) 13)

(define
  (dist-or x)
  (sqrt (+ (sqr (posn-x x)) (sqr (posn-y x)))))

(map dist-or (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5)))


; anchos: List (Images) -> List (Number)
; Dada una lista de imágenes, devuelve una lista con los anchos de
; las imágenes de la lista original
(check-expect (anchos (list (circle 30 "solid" "red")
                             (rectangle 10 30 "outline" "blue")))
               (list 60 10))
(check-expect (anchos '()) '())

(define
  (anchos l)
  (cond [(empty? l) empty]
        [else (cons (image-width (first l)) (anchos (rest l)))]
        ))

; Anchos: List (Images) -> List (Number)
; Dada una lista de imágenes, devuelve una lista con los anchos de
; las imágenes de la lista original
(check-expect (Anchos (list (circle 30 "solid" "red")
                             (rectangle 10 30 "outline" "blue")))
               (list 60 10))
(check-expect (Anchos '()) '())

(define (Anchos l) (map image-width l))


; eliminar: List Number -> List
; toma una lista de números y un número y devuelva la lista luego
; de eliminar el número que indica el 2do argumento
(check-expect (eliminar (list 1 2 3 2 7 6) 2)
              (list 1 3 7 6))
(check-expect (eliminar (list 1 2 3 2 7 6) 0)
              (list 1 2 3 2 7 6))

(define
  (eliminar l n)
  (cond [(empty? l) empty]
        [(= (first l) n) (eliminar (rest l) n)]
        [else (cons (first l) (eliminar (rest l) n))]
        ))


; pares: List (Number) -> List (Number)
; Toma una lista de números, y devuelve una nueva compuesta por los
; elementos pares de la primera
(check-expect (pares (list 4 6 3 7 5 0)) (list 4 6 0))

(define
  (pares l)
  (cond [(empty? l) empty]
        [(even? (first l)) (cons (first l) (pares (rest l)))]
        [else (pares (rest l))]
        ))

; Pares: List (Number) -> List (Number)
; Toma una lista de números, y devuelve una nueva compuesta por los
; elementos pares de la primera
(check-expect (Pares (list 5 6 3 7 5 0)) (list 6 0))

(define (Pares l) (filter even? l))

; mayores: List (Number) -> List (Number)
; dada una lista de números y un número devuelve una lista formada por
; aquellos elemntos de la lista original que son mayores al número dado
(check-expect (mayores (list 1 2 3 4 5) 4) (list 5))

(define
  (mayores l n)
  (cond [(empty? l) empty]
        [(> (first l) n) (cons (first l) (mayores (rest l) n))]
        [else (mayores (rest l) n)]
        ))

; todos-verdaderos: List (Bool) -> Bool
; dad una lista de booleanos, devuelve #true si todos los elementos son verd
; y #false en caso contrario
(check-expect (todos-verdaderos (list (= 1 1) (< 2 3) #true)) #true)
(check-expect (todos-verdaderos '()) '())

(define
  (todos-verdaderos l)
  (cond [(empty? l) empty]
        [(false? (first l)) #false]
        [(empty? (rest l)) #true]
        [else (todos-verdaderos (rest l))]))

(define (fand x y)
  (cond [(false? x) #f]
        [(false? y) #f]
        [else #t]))

(foldr fand '() (list #f #t #t))

; maximo: List (Number) -> Number
; dada una lista de números naturales devuelve el máximo
(check-expect (maximo (list 2233 3 4 567 8)) 2233)

(define
  (eliminaar l n)
  (cond [(empty? l) empty]
        [(= (first l) n) (eliminaar (rest l) n)]
        [else (cons (first l) (eliminaar (rest l) n))]
        ))



(define
  (maximo l)
  (cond [(empty? l) empty]
        [(empty? (rest l)) (first l)]
        [(< (first l) (second l)) (maximo (rest l))]
        [(> (first l) (second l)) (maximo (eliminaar l (second l)))]
        ))


(define
  (fmax x y)
  (if (< x y) y x))

(define (Maximo l) (foldr fmax 0 l))
(Maximo (list 2 3 44 3))

