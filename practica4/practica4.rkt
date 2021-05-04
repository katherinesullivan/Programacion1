;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define p (make-posn 3 4))
(define q (make-posn -2 0.5))

(posn-y (make-posn
(posn-x p) (posn-x q)))

; constructor:
; make-posn : Number Number -> posn

; selectores:
; posn-x : posn -> Number
; posn-y : posn -> Number

; predicado:
; posn?: t -> Bool

; Ejercicio 2

; Representamos puntos coordenados con la estructura posn y una distancia con un número
; dist-origen: posn -> Number

(define
  (dist-origen p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))


; Ejercicio 3

; Respresentamos puntos 2d con la estructura posn
; simetrico: posn -> posn

(check-expect (simetrico (make-posn 0 0)) (make-posn 0 0))
(check-expect (simetrico (make-posn -2 -2)) (make-posn 2 2))
(check-expect (simetrico (make-posn -3 4)) (make-posn 3 -4))
(check-expect (simetrico (make-posn 1 1.5)) (make-posn -1 -1.5))

(define
  (simetrico p)
  (make-posn (* -1 (posn-x p)) (* -1 (posn-y p))))

; Ejercicio 4

; Representamos puntos2d con la estructura posn y distancias con números
; dist-puntos : t -> String | Number

(check-expect (dist-puntos "hola" 2) "Tipos incorrectos para la función")
(check-expect (dist-puntos (make-posn 1 1) (make-posn 1 1)) 0)
(check-within (dist-puntos (make-posn (sqrt 2) (sqrt 2)) (make-posn 0 0)) 2.0 0.0001)
;(check-expect (dist-origen (make-posn (sqrt 2) (sqrt 2)) (make-posn (sqrt 2) (sqrt 2))) #i2.0)

(define
  (dist-puntos p q)
  (if (and (posn? p) (posn? q))
      (sqrt (+ (sqr (- (posn-x q) (posn-x p))) (sqr (- (posn-y q) (posn-y p)))))
      "Tipos incorrectos para la función"))

#|En este ejercicio moveremos un objeto verticalmente sobre
una escena. El estado del sistema será un número, que representa la
posición vertical sobre la que se va a dibujar un círculo. Defina
constantes para representar el ancho y alto de la escena, así como
también el radio inicial del círculo.|#

#|(require 2htdp/image)
(require 2htdp/universe)

(define ANCHO 300)
(define ALTO 300)
(define RADIO 15)

(define
  (dibujar s)
  (place-image (circle RADIO "solid" "red")
               (posn-x s) (posn-y s)
               (empty-scene ANCHO ALTO)))

; https://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=big-bang#%28tech._world._keyevent%29

(define DELTA 5)
(define INICIAL (make-posn (/ ANCHO 2)(/ ALTO 2)))

(define
  (manejarTeclado s k)
  (let* ([x (posn-x s)]
         [y (posn-y s)])
    (cond [(key=? k "up") (make-posn x (if (>= y (+ 0 DELTA RADIO)) (- y DELTA) y))]
          [(key=? k "down") (make-posn x (if (< y (- ALTO RADIO)) (+ y DELTA) y))]
          [(key=? k "right") (make-posn (if (< x (- ANCHO RADIO)) (+ x DELTA) x) y)]
          [(key=? k "left") (make-posn (if (>= x (+ 0 DELTA RADIO)) (- x DELTA) x) y)]
          [(key=? k " ") INICIAL]; 
          [else s])))

(define
  (mouse-handler s x y event)
  (cond [(string=? event "button-down") (make-posn x y)]
        [else s]))

(big-bang INICIAL
  [to-draw dibujar]
  [on-key manejarTeclado]
  [on-mouse mouse-handler])|#

; Ejercicio 6: La mosca

(require 2htdp/image)
(require 2htdp/universe)

#|(define ANCHO 300)
(define ALTO 300)
(define FONDO (empty-scene ANCHO ALTO))
(define INICIAL (make-posn (/ ANCHO 2) (/ ALTO 2)))
(define RADIO 10)
(define MOSCA (place-image (circle (/ RADIO 4) "solid" "white")
                           (/ RADIO 2) (/ RADIO 2)
                           (circle RADIO "solid" "black")))
(define DELTA 10)
(define GAMMA 10)

; Representamos el estado con una estructura posn
; interpretar: posn -> Image
; La función dibuja la mosca en la posición indicada por el estado
; o el texto MOSCA ATRAPADA si el estado es (-1,-1)
(define
  (interpretar s)
  (if (and (= (posn-x s) -1) (= (posn-y s) -1))
      (place-image/align (text "MOSCA ATRAPADA" 30 "black")
                         (/ ANCHO 2) (/ ALTO 2)
                         "center" "center"
                         FONDO)
      (place-image MOSCA
                   (posn-x s) (posn-y s)
                   FONDO)))



#|PARA MOVERSE|#

; Representamos acciones con a y b
; elegir-random: accion accion -> accion
; Dadas dos acciones ejecuta aleatoriamente la una o la otra
(define (elegir-random a b) (if (= (random 2) 0) a b))

; Representamos el estado con una estructura posn
; elegir-random-izq-der: posn -> Number
; Suma o resta (en caso de que la mosca no se vaya de la escena) DELTA unidades
; a la coordenada x de la posición y devuelve esta misma
(define
  (elegir-random-izq-der s)
  (elegir-random (if (< (posn-x s) (- ANCHO RADIO)) (+ DELTA (posn-x s)) (posn-x s))
                 (if (>= (posn-x s) (+ 0 RADIO DELTA)) (- (posn-x s) DELTA) (posn-x s))
                 )
  )

; Representamos el estado con una estructura posn
; elegir-random-arr-abajo: posn -> Number
; Suma o resta (en caso de que la mosca no se vaya de la escena) DELTA unidades
; a la coordenada y de la posición y devuelve esta misma
(define
  (elegir-random-arr-abajo s)
  (elegir-random (if (< (posn-y s) (- ALTO RADIO)) (+ DELTA (posn-y s)) (posn-y s))
                 (if (>= (posn-y s) (+ 0 RADIO DELTA)) (- (posn-y s) DELTA) (posn-y s))
                 )
  )

; Representamos el estado con una estructura posn
; moverse: posn -> posn
; La funcion aleatoriamente mueve la posición de la mosca
; DELTA unidades a la derecha o a la izquierda y
; DELTA unidades arriba o abajo

(define
  (moverse s)
  (make-posn
   (elegir-random-izq-der s)
   (elegir-random-arr-abajo s)))

#|FIN MOVERSE|#

; Representamos el estado con una estructura posn, la posicion del mouse con 2 números
; check-atrapada: posn Number Number -> posn
; Se fija si los puntos se encuentran a una distancia menor a GAMMA, si es así devulve
; (-1,-1) si no devuelve el posn pasado como argumento

(define
  (check-atrapada s x y)
  (if (< (dist-puntos s (make-posn x y)) GAMMA)
      (make-posn -1 -1)
      s))

; Representamos el estado con una estructura posn, la posicion del mouse con 2 números
; y el evento de mouse con un string
; atrapada?: posn Number Number String -> posn
; Si el lugar donde se hizo click está a una distancia menor que GAMMA de la mosca
; devuelve el estado (-1,-1), caso contrario devuelve el mismo estado

(define
  (atrapada? s x y event)
  (if (string=? event "button-down")
      (check-atrapada s x y)
      s))
      
; Representamos el estado con una estructura posn
; atrapada: posn -> Bool
; Devuelve true cuando la posición de la mosca es (-1,-1)
(define
  (atrapada s)
  (and (= (posn-x s) -1) (= (posn-y s) -1)))

(big-bang INICIAL
  [to-draw interpretar]
  [on-tick moverse 5]
  [on-mouse atrapada?]
  [stop-when atrapada] ; si la comento es que funciona lo de mosca atrapada
  )|#

; Ejercicio 8

(define-struct Texto [s color tam])
; Texto es (String, Color, Number)
; Intepretación: El primer elemento es la cadena a mostarse, mientras que el segundo y
; el tercero determinan el color y tamaño de la fuente en píxeles respectivamente.

(define MIN-TAM 8)
(define MAX-TAM 35)
(define WIDTH 800)
(define HEIGHT 60)
(define FONT-COLOR-INICIAL "black")
(define FONT-SIZE-INICIAL 15)
(define INICIAL (make-Texto "" FONT-COLOR-INICIAL FONT-SIZE-INICIAL))
(define COLOR-F1 "black")
(define COLOR-F2 "blue")
(define COLOR-F3 "green")
(define COLOR-F4 "red")
(define COLOR-F5 "yellow")
(define COLOR-F6 "gray")

(define
  (draw texto)
  (place-image/align (text (Texto-s texto) (Texto-tam texto) (Texto-color texto))
                     0 0 "left" "top"
                     (empty-scene WIDTH HEIGHT)))

(define
  (manejarTeclado t k)
  (cond [(number? (string->number k))
         (make-Texto (string-append (Texto-s t) k) (Texto-color t) (Texto-tam t))]
        [(and (=(string-length k)1)(char-alphabetic? (string-ref k 0)))
         (make-Texto (string-append (Texto-s t) k) (Texto-color t) (Texto-tam t))]
        [(key=? k " ")
         (make-Texto (string-append (Texto-s t) k) (Texto-color t) (Texto-tam t))]
        [(key=? k "\b")
         (make-Texto (substring (Texto-s t) 0 (- (string-length (Texto-s t)) 1)) (Texto-color t) (Texto-tam t))]
        [(key=? k "up") (if (< (Texto-tam t) MAX-TAM)
                            (make-Texto (Texto-s t) (Texto-color t) (add1 (Texto-tam t)))
                            t)]
        [(key=? k "down") (if (> (Texto-tam t) MIN-TAM)
                            (make-Texto (Texto-s t) (Texto-color t) (sub1 (Texto-tam t)))
                            t)]
        [(key=? k "f1") (make-Texto (Texto-s t) COLOR-F1 (Texto-tam t))]
        [(key=? k "f2") (make-Texto (Texto-s t) COLOR-F2 (Texto-tam t))]
        [(key=? k "f3") (make-Texto (Texto-s t) COLOR-F3 (Texto-tam t))]
        [(key=? k "f4") (make-Texto (Texto-s t) COLOR-F4 (Texto-tam t))]
        [(key=? k "f5") (make-Texto (Texto-s t) COLOR-F5 (Texto-tam t))]
        [(key=? k "f6") (make-Texto (Texto-s t) COLOR-F6 (Texto-tam t))]
        [else t]
        ))

(big-bang INICIAL
  [to-draw draw]
  [on-key manejarTeclado]
  )

; Ejercicio 12

(define-struct Persona [name w mw h mh])
; Persona es String Number String Number String
; Intepretación: El primer elemento es el nombre y apellido de la persona,
; el segundo su peso en numero, el tercero la unidad de medida de su peso,
; el cuarto su estatura en numeros y el quinto la unidad de medioda de su estatura

(define
  (peso-kg peso unidad)
  (cond [(string=? unidad "kg") peso]
        [(string=? unidad "g") (/ peso 1000)]
        [else "Se rompe"]))

(define
  (estatura-m estatura unidad)
  (cond [(string=? unidad "m") estatura]
        [(string=? unidad "cm") (/ estatura 100)]
        [else "Se rompe"]))

(define
  (IMC p)
  (if (Persona? p)
      (let* ([peso (peso-kg (Persona-w p) (Persona-mw p))]
             [estatura (estatura-m (Persona-h p) (Persona-mh p))])
        (/ peso (sqr estatura))
      )
      "Tipo de dato inválido"))


