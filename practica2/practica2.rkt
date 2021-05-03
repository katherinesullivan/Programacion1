;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Representamos temperaturas mediante números.
; Number -> Number
; La función recibe una temperatura en Fahrenheit y devuelve su equivalente en Celsius.

; Ejemplos:
; entrada: 32, salida: 0
; entrada: 212, salida: 100
; entrada: 122, salida: 50

(define
  (far->cel t)
  (* 5/9 (- t 32)))


#|Diseñe una función distancia-origen , que recibe dos
números x e y, devolviendo como resultado la distancia al origen del
punto (x,y).|#

; Representamos las coordenadas de un punto 2d mediante dos números y una distancia mediante un número
; Number Number -> Number
; La función recibe un punto 2d y devuelve la distancia del mismo al origen

; Ejemplos:
; entrada: 1 1, salida: (sqrt 2)
; entrada: 2 2, salida: (sqrt 8)
; entrada: 0 0, salida: 0
; entrada: -2 4, salida: (sqrt 20)
; entrada: (sqrt 2) (sqrt 2), salida: 2
; entrada: 3 4, salida 5
; entrada: -3 4, salida: 5
; entrada: -4 -3, salida: 5

#|Inexact numbers print with a decimal point or exponent specifier,
and exact numbers print as integers and fractions.
The same conventions apply for reading number constants,
but #e or #i can prefix a number to force its parsing as an exact or inexact number.
The prefixes #b, #o, and #x specify binary, octal, and hexadecimal interpretation of digits.
In https://docs.racket-lang.org/guide/numbers.html|#

(define
  (distancia-origen x y)
  (sqrt (+ (sqr (- x 0)) (sqr (- y 0)))))

(distancia-origen (sqrt 2) (sqrt 2))

#|Diseñe una función distancia-puntos , que recibe cuatro
números x1, y1, x2 e y2 y devuelve la distancia entre los puntos (x1,
y1) y (x2, y2)|#

; Representamos las coordenadas de un punto2d a través de dos números y la distancia a través de uno
; Number Number Number Number -> Number
; La función recibe las coordenadas de dos puntos y devuelve la distancia entre ellos

; Ejemplos
; entrada: 1 1 0 0, salida: (sqrt 2)
; entrada: 2 2 4 4, salida: (srqt 8)
; ..

(define
  (distancia x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

(define
  (vol-cubo l)
  (expt l 3))

(define
  (area-cubo l)
  (* 6 (sqr l)))

; STRINGS

#|Diseñe la función string-insert , que consume un string y
un número i e inserta "-" en la posición i-ésima del string|#

; Representamos posiciones de caracteres en un string medainte números
; String Number -> String
; La función toma un string y una posicion de la string y devulve la string con el caracter "-" en la posición tomada

; Ejemplos:
; entrada: hola 2, salida: ho-la

(define
  (string-insert str i)
  (string-append (substring str 0 i) "-" (substring str i)))

; Ejemplos:
; entrada: hola 2, salida: ho-a

#|procedure

(string-set! str k char) → void?

  str : (and/c string? (not/c immutable?))
  k : exact-nonnegative-integer?
  char : char?
Changes the character position k in str to char.
The first position in the string corresponds to 0,
so the position k must be less than the length of the string, otherwise the exn:fail:contract exception is raised.
|#

#|(define
  (string-insert2 str k)
  (string-set! str k #\-))|#
; this one would be without ayuda lang intermedio y con non inmutable strings

#|Diseñe la función string-last , que extrae el último
caracter de una cadena no vacía.|#

; String -> Char
; La función toma un string y devuelve su último caracter

(define
  (string-last str)
  (string-ref str (- (string-length str) 1)))

(string-last "hola")

(define
  (string-lastNO str)
  (substring str (- (string-length str) 1)))

(string-lastNO "hola")

#|Diseñe la función string-remove-last , que recibe una
cadena y devuelve la misma cadena sin el último caracter|#

; String -> String
; La función toma un string y devuelve el mismo sin su último carácter

(define
  (string-remove-last str)
  (substring str 0 (sub1 (string-length str))))

(string-remove-last "hola")


; NOW WITH CHECK-EXPECT

#|Si se anotan 2 amigos, cada uno obtiene un 10% de descuento sobre
el valor de la cuota; mientras que si se anotan 3 o más el descuento
alcanza el 20%
Si al momento de pagar se decide abonar 2 meses juntos se recibe
un descuento del 15%; en caso de cancelar 3 o más meses a la vez la
reducción es del 25%
Las promociones son combinables, pero nunca pueden superar el 35%
de descuento. El valor original de la cuota mensual es de $650.|#

; Representamos la cantidad de personas, el número de meses por los cuales se anotan y el monto de dinero con números
; Number Number -> Number
; La función toma la cantidad de personas que se anotan juntas y la cantidad de meses por lo que lo hacen
; y devuelve el monto que debe pagar cada persona

(define CUOTA_MENSUAL 650)

(check-expect (monto-persona 2 2) 975)
(check-expect (monto-persona 3 3) 1267.50)
(check-expect (monto-persona 1 5) 2437.50)

#|(define
  (descuentos-acumulables personas meses)
  (let* ([monto (* meses CUTO_MENSUAL)]
         [monto_new (if (<= 3 personas)
                        (* monto 0.8)
                        (if (= 2 personas)
                            (* 0.9 monto) monto))
                    (if (<= 3 meses)
                        (* monto |#

(define
  (monto-persona personas meses)
  (cond [(and (<= 3 personas) (<= 2 meses)) (* (* meses CUOTA_MENSUAL) 0.65)] ; Máx desc 35%
        [(and (= 2 personas) (<= 3 meses)) (* (* meses CUOTA_MENSUAL) 0.65)]
        [(and (= 2 personas) (= 2 meses)) (* (* meses CUOTA_MENSUAL) 0.75)]
        [(<= 3 personas) (* (* meses CUOTA_MENSUAL) 0.80)]
        [(= 2 personas) (* (* meses CUOTA_MENSUAL) 0.90)]
        [(<= 3 meses) (* meses CUOTA_MENSUAL 0.75)]
        [(= 2 meses) (* meses CUOTA_MENSUAL 0.85)]
        [else (* meses CUOTA_MENSUAL)]))
        
#|edad <= 1 mes: nivel mínimo de hemoglobina normal 13 g/dl
1 mes < edad <= 6 meses: nivel mínimo de hemoglobina normal
10 g/dl
6 meses < edad <= 12 meses: nivel mínimo de hemoglobina
normal 11 g/dl
1 año < edad <= 5 años: nivel mínimo de hemoglobina normal
11.5 g/dl
5 años < edad <= 10 años: nivel mínimo de hemoglobina normal
12.6 g/dl
10 años < edad: nivel mínimo de hemoglobina normal 13 g/dl|#

; Representamos la edad en meses con un número y el nivel de hemoglobina en sangre expresado en g/dl con otro número
; Number Number -> String
; Si el nivel de hemoglobina que tiene una persona es menor que el valor
; mínimo que le corresponde de acuerdo a su edad, el resultado del
; análisis es "anemia positivo" y en caso contrario es "anemia negativo"

(check-expect (anemia 1 12) "anemia positivo")
(check-expect (anemia 14 14.9888888) "anemia negativo")
(check-expect (anemia 120 12.01) "anemia positivo")

(define
  (categorizar-edad meses)
  (cond [(<= meses 1) 1]
        [(<= meses 6) 2]; ya sabemos que va a tener más de 1 mes pq no evaluo true el predicado anterior
        [(<= meses 12) 3]
        [(<= meses 60) 4]
        [(<= meses 120) 5]
        [else 6]))

(define
  (anemia meses hemog)
  (let ([cat_edad (categorizar-edad meses)])
    (cond [(= cat_edad 1) (if (< hemog 13) "anemia positivo" "anemia negativo")]
          [(= cat_edad 2) (if (< hemog 10) "anemia positivo" "anemia negativo")]
          [(= cat_edad 3) (if (< hemog 11) "anemia positivo" "anemia negativo")]
          [(= cat_edad 4) (if (< hemog 11.5) "anemia positivo" "anemia negativo")]
          [(= cat_edad 5) (if (< hemog 12.6) "anemia positivo" "anemia negativo")]
          [(= cat_edad 6) (if (< hemog 13) "anemia positivo" "anemia negativo")])))

#|Decimos que una terna de números a,b,c es
autopromediable si uno de sus valores concide con el promedio de los
otros dos|#
#|Diseñe una función que dados tres números, devuelva el producto de
ellos en caso que formen una terna autopromediable, y la suma de los
mismos en caso contrario.|#

; Number Number Number -> Number
; arriba

(check-expect (prom 5 7 9) (* 5 7 9))
(check-expect (prom 5 7.3 9) 21.3)
(check-expect (prom 0 1 2) 0)
(check-expect (prom 0 1.1 2) 3.1)

(define
  (autopromediable? a b c)
  (let ([promedio (/(+ a b c) 3)])
    (cond [(= promedio a) #true]
          [(= promedio b) #true]
          [(= promedio c) #true]
          [else #false]))) ; si no con ors
(define
  (prom a b c)
  (if (autopromediable? a b c) (* a b c) (+ a b c)))

#|Diseñe una función autonomía , que dados los siguientes argumentos:
La cantidad de litros restantes en el tanque de combustible, y
La clase de combustible que se está utilizando,
devuelva un string indicando la autonomía del auto, tanto en ciudad
como en ruta|#

#|8km/l en ciudad y 11km/l en ruta
Al cargar combustible con nafta grado 3 (conocida
como "premium"), el rendimiento mejora un 10%|#

; Representamos los litros de combustible, la clase de combustible y los km/l de autonomía con números
; Number Number -> String

(check-expect (autonomia 20 2) "Autonomía en ciudad: 160km. Autonomía en ruta: 220km.")
(check-expect (autonomia 20 3) "Autonomía en ciudad: 176km. Autonomía en ruta: 242km.")
(check-expect (autonomia 4 1) "No existe la clase de combustible ingresada")
(check-expect (autonomia 0 2) "Autonomía en ciudad: 0km. Autonomía en ruta: 0km.")
(check-expect (autonomia 0 3) "Autonomía en ciudad: 0km. Autonomía en ruta: 0km.")

(define
  (autonomia2 litros)
  (string-append "Autonomía en ciudad: " (number->string (* 8 litros)) "km. " "Autonomía en ruta: "
                 (number->string (* 11 litros)) "km."))

(define
  (autonomia3 litros)
  (string-append "Autonomía en ciudad: " (number->string (* 8 litros 1.1)) "km. " "Autonomía en ruta: "
                 (number->string (* 11 litros 1.1)) "km."))

(define
  (autonomia litros clase)
  (cond [(= clase 2) (autonomia2 litros)]
        [(= clase 3) (autonomia3 litros)]
        [else "No existe la clase de combustible ingresada"]))
