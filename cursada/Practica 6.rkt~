;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Practica 6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Práctica 6

; Ejercicio 1
; sumanat: Natural Natural -> Natural
; toma dos números naturales y , sin usar el operador +, devuelve su suma

(define
  (sumanat x y)
  (cond [(zero? x) y]       ; op 1 cond (and (positive? x y el de y
        [(zero? y) x]
        [else (add1 (sumanat (sub1 x) y))]))
;(sumanat 7 4)

; Ejercicio 2
; multiplicar: Natural Natural -> Natural
; toma como entrada dos números naturales y los multiplica sin usar *

(define
  (multiplicar x y)
  (cond [(= 1 x) y]
        [(= 1 y) x]
        [else (sumanat y (multiplicar (sub1 x) y))])); puede ser + en vez
;(multiplicar 7 3)

;Ejercicio 3
; powernat: Natural Natural -> Natural
; toma dos números naturales y devuelve el resultado de elevar el primero
; a la potencia del segundo, sin usar expt

(define
  (powernat x y)
  (cond [(= 1 x) x]
        [(= 1 y) x]
        [else (multiplicar x (powernat x (sub1 y)))]))
;(powernat 4 2)

; Ejercicio 3
; Sigma: Natural f(Natural -> Number) -> Number
; dados un número natural n y una función f, devuelve la sumatoria de
; todos los valores entre f (0) y f (n)

(define
  (Sigma n f)
  (cond [(zero? n) (f n)]
        [else (+ (f n) (Sigma (sub1 n) f))]))
;(Sigma 4 identity)

; Ejercico 5
; VER CHECK-WITHIN

; G: Natural -> Number

(define
  (G i)
  (/ 1 (expt i 3)))

; R: Natural -> Number

(define
  (R i)
  (/ 1 (expt 2 i)))

; S: Natural -> Number

(define
  (S i)
  (/ i (+ i 1)))

; T: Natural -> Number

(define
  (T i)
  (/ 1 (+ i 1)))

; Ejercicio 6
; intervalo: Natural -> List (Natural)
; dado un natural n, devuelve una lista conformada por todos los naturales
; hasta n

(define
  (intervalo n)
  (cond [(zero? n) empty]
        [else (cons n (intervalo (sub1 n)))]))
; DONDE PONER EL REVERSE DENTRO DE LA FUNCIÓN
;(intervalo 4);!!!!!!!

; Ejercicio 7
; factnat: Natural -> Natural
; dado un número n, facnat calcula su factorial

(define
  (factnat n)
  (cond [(or (= n 0) (= n 1)) 1]
        [else (* (factnat (sub1 n)) n)]))
;(factnat 4)

; Ejercicio 8
; fibnat: Natural -> Number
; dado un  número n, fibnat devuelve el valor correspondiente a la sucesión
; de Fibonacci para ese valor.

(define
  (fibnat n)
  (cond [(or (= n 0) (= n 1)) 1]
        [else (+ (fibnat (- n 2)) (fibnat (- n 1)))]))
;(fibnat 5)

; Ejercicio 9
; list-fibonacci: Number -> List (Number)
; dado un número n, devuelve una listacon los numeros correspondientes
; a la de fibonacci para todos los números anteriores al n y el n dado

(define
  (list-fibonacci n)
  (cond [(= n 0) (list 1)]
        [(= n 1) (list 1 1)]
        [else (cons (fibnat n) (list-fibonacci (fibnat (sub1 n))))]))
;(list-fibonacci 4)

; Ejercicio 10.1
; g: Natural -> Natural
; dado un número n devuelve 1 si es 0, 2 si es 1, 3 si es dos, y si es mayor
; a 3, devuelve el producto de los tres números que le preceden

(define
  (g n)
  (cond [(zero? n) 1]
        [(= n 1) 2]
        [(= n 2) 3]
        [else (* (g (sub1 n)) (g (sub1 (sub1 n))) (g (- n 3)))]))
;(g 3)

; Ejercicio 10.2
; list-g: Natural -> List
; dado un número n, devuelve la lista formada con el resultado de evaluar
; todos los naturales con g, de 0 a n

(define
  (g-list n)
  (cond [(= n 0) (list 1)]
        ;[(= n 1) (list 2 1)] ;118 vs 115
        ;[(= n 2) (list 3 2 1)] ; 115 vs 106 
        [else (cons (g n) (g-list (sub1 n)))]))
;(g-list 4)

; Ejercicio 11
; componer: f(Number -> Number) Natural Number -> Number
; devuelve el resultado de aplicar n veces la función f a x

(define
  (componer f n x)
  (cond [(= n 0) 0]
        [(= n 1) (f x)]
        [else (f (componer f (sub1 n) x))]))
;(componer sqr 2 5)

; Ejercicio 12
; multiplos: Natural Natural -> List
; dado un natural n y otro m, devuelve una list con los primeros n
; multiplos de m

(define
  (multiplos n m)
  (cond [(= n 0) empty]
        [else (cons (* m n) (multiplos (sub1 n) m))]))
;(multiplos 4 7)

; Ejercicio 13
; G: Natural f(Natural -> Bool) -> Bool
; dado un número natural, y una función que tomando un número, devuelve
; un booleano. G devuelve true si alguno de los valores de 0 a n
; valuado en la función dada, es verdadero. En caso contrario, devolverá falso

(define
  (Gg n f)
  (cond [(= n 0) (f 0)]
        [else (or (f n) (Gg (sub1 n) f))]))
;(Gg 3 negative?)

; Ejercicio 14
; circulos: Natural -> Image
; dado un número natural m, circulos devuelve una imagen de 2m2 x 2m2 con m
; círculos azules centrados en el centro de la imagen y radios que van de 1

(define
  (circulos m)
  (cond [(= m 0) (empty-scene 200 200)]
        [(= m 1) (place-image (circle (sqr m) "outline" "blue")
                              100 100
                              (empty-scene 200 200))]
        [else (overlay (circle (sqr m) "outline" "blue")
                       (circulos (sub1 m)))]))
(circulos 10)

; Ejercicio 15
; cuadrados: Natural Number -> Image
; dado un número natural m y un ángulo ang, cuadrados devuelve
; una una imagen de 200 x 200 con m cuadrados azules centrados
; en el centro de la imagen.  Los lados de los cuadrados serán lados: m2 ,
; (m-1)2, ... , 12. El ángulo ang indica la rotación del cuadrado de mayor
; dimensión. El ángulo que corresponde al cuadrado de lado (m-1)2 debe ser
; 20 grados más que el que le corresponde al cuadrado de lado m2,
; para cualquier valor de m mayor o igual a 1.

(define
  (cuadrados m ang)
  (cond [(= m 0) (empty-scene 200 200)]
        [(= m 1) (place-image (rotate ang
                               (square (sqr m) "outline" "blue"))
                           100 100
                           (empty-scene 200 200))]
        [else (overlay (rotate ang
                               (square (sqr m) "outline" "blue"))
                       (cuadrados (sub1 m) (+ ang 20)))]))
;(cuadrados 10 70)

; Ejercicio 16
; cuotas: Number Natural Number -> List (Number)
; dado un importe de un préstamo, total, un valor correspondiente
; al número de cuotas, n, y una tasa de interés, i, devuelva una lista
; con las cuotas a pagar ordenadas de forma creciente. Las cuotas se
; calcularán, sumando total/n y (total/n) * (i/ (100*12)) * j, siendo j
; el número que representa la cuota a pagar

(define
  (cuotas total n i)
  (cond [(= n 0) empty]
        [(= n 1) (cons
                  (+ (/ total n) (* (/ total n) (/ i (* 100 12)) 1)) '())]
        [else (cons (+ (/ total (+ n 1)) (* (/ total (+ n 1))
                                           (/ i (* 100 12)) n))
                    (cuotas total (sub1 n) i))]))

;(cuotas 30000 3 12)
;(check-expect (cuotas 10000 0 18)
              ;'())
;(check-expect (cuotas 10000 1 12)
              ;(list 10100))
;(check-expect (cuotas 30000 3 12)
              ;(list 10100 10200 10300))
;(check-expect (cuotas 100000 4 18)
              ;(list 25375 25750 26125 26500))

