;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define W 200)
(define H 200)
(define radius 15)

(require 2htdp/image)
(require 2htdp/universe)

#|(big-bang < estado inicial >
     [to-draw < controlador de pantalla >]
     [on-key < manejador de teclado >]
     [on-mouse < manejador de mouse >]
     [on-tick < manejador de reloj >]
     [stop-when < predicado de fin de ejecución >]
     etc..
   )|#

#|
Tenemos entonces que big-bang recibe dos argumentos: el estado
inicial y una lista de parejas. De estas parejas, sólo una es obligatoria
( to-draw ... ). Esta primera pareja nos indica cómo interpretar el
estado actual (en nuestro caso, como una imagen, y la función
interpretar de más arriba nos sirve).
|#

#|
Cuando se evalúa, el comportamiento de esta expresión es como sigue:
1. La función asociada a to-draw es invocada con el estado inicial
como argumento, y su resultado se muestra por pantalla.
2. El programa queda a la espera de un evento.
3. Cuando un evento ocurre, el manejador asociado a dicho evento (si
existe) es invocado, y devuelve el nuevo estado.
4. En caso de estar presente, se aplica el predicado (la condición)
asociado a la clásula stop-when al nuevo estado. Si devuelve #true ,
el programa termina, si no,
5. la función asociada a to-draw es nuevamente invocada con el nuevo
estado, devolviendo la nueva imagen o escena.
6. El programa queda a la espera de un nuevo evento (volvemos al
paso 2).
|#

#|; interpretar: Estado -> Image
; dado un estado, devuelve la imagen a mostrar por el programa
; (omitimos los ejemplos para faciltar la legibilidad)
(define
  (interpretar s)
  (place-image (circle 10 "solid" s) (/ W 2) (/ H 2) (empty-scene W H)))

(interpretar "red")

; manejarTeclado: Estado String -> Estado
(define
  (manejarTeclado s k)
  (cond [(key=? k "a") "blue"]
        [(key=? k "r") "red"]
        [(key=? k "v") "green"]
        [(key=? k " ") "blue"] ; inicial ; si no " "
        [(key=? k "n") "black"]
        [else s]))

#|Siempre que asociemos una función al evento on-key , esta tendrá
dos argumentos: el estado actual y un string que representa la tecla
que generó el evento. Y siempre devolverá un nuevo estado.|#

(big-bang "blue"
  [to-draw interpretar]   ; estado inicial del sistema
                          ; dibuja en la pantalla
                          ; el círculo en el estado actual
  [on-key manejarTeclado]); cuando se presiona una tecla,
                          ; la función manejarTeclado
                          ; se invoca para manejar el evento

; manejarReloj: Estado -> Estado
; manejarMouse: Estado Number Number String -> Estado|#

#|(define
  (pant n color)
    (place-image (circle n "solid" color)
               150 150
               (empty-scene 300 300)))
; pantalla : Number -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw
(define
  (pantalla n)
  (cond [(<= n 50) (pant n "yellow")]
        [(<= n 100) (pant n "red")]
        [else (pant n "green")]))

; decrementar : Number -> Number
; Devuelve el predecesor de un número positivo.
; Si el número es 0, devuelve 100.
(define
  (decrementar n)
  (if (= n 0) 100 (sub1 n)))

; incrementar : Number -> Number
; suma uno a su argumento
(define
  (incrementar n)
  (if (>= n 150) 0 (+ n 1)))

(define
  (terminar? n)
  (or (> n 110) (< n 10)))

(define
  (manejarTeclado n k)
  (let ([d (string->number k)])
    (if (number? d) (* n d) n)))

(big-bang 100; estado inicial del sistema
  [to-draw pantalla]
  [on-tick incrementar]
  [on-key manejarTeclado]
  [stop-when terminar?]
  )|#

#|En este ejercicio moveremos un objeto verticalmente sobre
una escena. El estado del sistema será un número, que representa la
posición vertical sobre la que se va a dibujar un círculo. Defina
constantes para representar el ancho y alto de la escena, así como
también el radio inicial del círculo.|#

#|(define ANCHO 300)
(define ALTO 300)
(define RADIO 15)

(define
  (dibujar s)
  (place-image (circle RADIO "solid" "red")
               (/ ANCHO 2) s
               (empty-scene ANCHO ALTO)))

#|Al presionar la "flecha para arriba", el objeto debe subir DELTA
unidades, donde DELTA es una constante que también debe definir.
Análogamente, el objeto debe "bajar" DELTA unidades si se presiona
la tecla "flecha para abajo"|#
; https://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=big-bang#%28tech._world._keyevent%29

(define DELTA 5)
(define INICIAL (/ ALTO 2))

(define
  (manejarTeclado s k)
  (cond [(key=? k "up") (if (>= s (+ 0 DELTA RADIO)) (- s DELTA) s)]
        [(key=? k "down") (if (< s (- ALTO RADIO)) (+ s DELTA) s)]
        [(key=? k " ") INICIAL]; 
        [else s]))

(define
  (mouse-handler n x y event)
  (cond [(string=? event "button-down") y]
        [else n]))

(big-bang INICIAL
  [to-draw dibujar]
  [on-key manejarTeclado]
  [on-mouse mouse-handler])|#

#|Este programa comienza con el fondo de color blanco y un
círculo en el centro de la escena. A medida que pasen los ticks del reloj
irá cambiando el color del círculo central. Pasará por los siguientes
colores: amarillo, rojo, verde y azul, en el orden presentado.
El estado del sistema guardará el color del círculo central de la escena|#

#|(define ANCHO 300)
(define ALTO 100)
(define RADIO 15)
(define INICIAL "yellow")

(define
  (dibujar color)
  (place-image (circle RADIO "solid" color)
              (/ ANCHO 2) (/ ALTO 2)
              (empty-scene ANCHO ALTO)))

;amarillo, rojo, verde y azul
(define
  (cambia-color color)
  (cond [(string=? color "yellow") "red"]
        [(string=? color "red") "green"]
        [(string=? color "green") "blue"]
        [(string=? color "blue") "yellow"]))

(big-bang INICIAL
  [to-draw dibujar]
  [on-tick cambia-color 1.5]
  )|#

#|(define WIDTH 800)
(define HEIGHT 60)
(define FONT-SIZE 20)
(define FONT-COLOR "indigo")

(define
  (draw str)
  (place-image/align (text str FONT-SIZE FONT-COLOR)
                     0 0 "left" "top"
                     (empty-scene WIDTH HEIGHT)))

(define
  (manejarTeclado str k)
  (cond [(number? (string->number k)) (string-append str k)]
        [(and (=(string-length k)1)(char-alphabetic? (string-ref k 0))) (string-append str k)]
        [(key=? k " ") (string-append str k)]
        [(key=? k "\b") (substring str 0 (- (string-length str) 1))]
        [else str]
        ))

(big-bang ""
  [to-draw draw]
  [on-key manejarTeclado]
  )|#

#|(define AUTO (bitmap "/home/administrador/Descargas/car.jpg"))
(define WIDTH 800)

(define
  (draw x)
  (place-image AUTO x 40 (empty-scene WIDTH 80)))

(define
  (mover-3px x)
  (if (<= (+ x 3) (- WIDTH 26))
      (+ x 3)
      (+ (- WIDTH 26 x) x)))

(define
  (manejadorTeclado x k)
  (cond [(key=? k " ") 26]
        [(key=? k "right") (if (>= (+ x 20) (- WIDTH 26)) (- WIDTH 26) (+ x 20))]
        [(key=? k "left") (if (<= (- x 20) 26) 26 (- x 20))]
        [else x]))

(define
  (manejadorMouse s x y event)
  (cond [(string=? event "button-down") x]
        [else s]))


(big-bang 26 ; 50 mide el auto / 2
  [to-draw draw]
  [on-tick mover-3px]
  [on-key manejadorTeclado]
  [on-mouse manejadorMouse]
  )|#

#|(define ANCHO 300)
(define ALTO 300)
(define COLOR "blue")
(define STAR-COLOR "white")
(define STAR-SIZE 0.1)

(define FONDO (empty-scene ANCHO ALTO COLOR))

(define
  (draw imagen)
  imagen)

(define
  (addstar x y image)
  (if (and (<= (+ x STAR-SIZE) ANCHO) (>= (- x STAR-SIZE) 0)
          (<= (+ y STAR-SIZE) ALTO) (>= (- y STAR-SIZE) 0))
      
      (place-image (star STAR-SIZE "solid" STAR-COLOR)
                   x y
                   image)
      image))

(define
  (addstar2 x y image)
  (let ([new_size (* STAR-SIZE x)])
    (if (and (<= (+ x new_size) ANCHO) (>= (- x new_size) 0)
          (<= (+ y new_size) ALTO) (>= (- y new_size) 0))
        (place-image (star new_size "solid" STAR-COLOR)
                     x y
                     image)
        image)))

(define
  (mouse-handler image x y event)
  (cond [(string=? event "button-down") (addstar2 x y image)]
        [else image]))

(define
  (key-handler image k)
  (if (key=? k "\b") FONDO image))

(big-bang FONDO
  [to-draw draw]
  [on-mouse mouse-handler]
  [on-key key-handler]
  )
|#

#|En este ejercicio dibujaremos un objeto sobre una escena.
Los posibles objetos a dibujar son un círculo o un triángulo, y estos
pueden ser de color azul o verde. Para representar el estado
utilizaremos un string, con la convención de que la primera letra
representa la figura a dibujar ( "t" o "c" ), y la segunda letra el color de
la figura ( "a" o "v" ).|#

#|(define ANCHO 300)
(define ALTO 300)
(define COLOR "white")
(define DIM 40)
(define FONDO (empty-scene ANCHO ALTO COLOR))

(define
  (draw-circle color)
  (place-image (circle DIM "solid" color)
                      (random (+ 1 ANCHO)) (random (+ 1 ALTO))
                      FONDO))

(define
  (draw-triangle color)
  (place-image (triangle DIM "solid" color)
                      (random (+ 1 ANCHO)) (random (+ 1 ALTO))
                      FONDO))

(define
  (draw str)
  (cond [(string=? str "tv") (draw-triangle "green")]
        [(string=? str "ca") (draw-circle "yellow")]
        [(string=? str "cv") (draw-circle "green")]
        [(string=? str "ta") (draw-triangle "yellow")]))

(define
  (figure str)
  (let ([color (if (equal? #\a (string-ref str 1)) "yellow" "green")])
    (if (equal? #\t (string-ref str 0))
        (place-image (triangle DIM "solid" color)
                 (random (+ 1 ANCHO)) (random (+ 1 ALTO))
                 FONDO)
        (place-image (circle DIM "solid" color)
                 (random (+ 1 ANCHO)) (random (+ 1 ALTO))
                 FONDO)
        )))

(define
  (key-handler str k)
  (cond [(key=? k "t") (string-append "t" (substring str 1))]
        [(key=? k "c") (string-append "c" (substring str 1))]
        [else str]))

(define
  (cambia-color str)
  (if (equal? #\a (string-ref str 1)) (string-append (substring str 0 1) "v") (string-append (substring str 0 1) "a")))

(big-bang "tv"
  [to-draw figure]
  [on-key key-handler]
  [on-tick cambia-color 1]
  )|#

(define IZQ (rotate 270 (bitmap "/home/administrador/Descargas/pie-izq.jpg")))
(define DER (rotate 270 (bitmap "/home/administrador/Descargas/pie-der.jpg")))
IZQ
DER

#|Se pide escribir un programa que simule a una persona caminando en
línea recta de izquierda a derecha o de derecha a izquierda sobre una
imagen vacía, a razón de un número impar de píxeles con cada tick del
reloj. Tener en cuenta la orientación de las huellas según el sentido en
que se avanza. Si llega a un extremo de la escena se considera que la
persona gira y se dirige hacia el otro extremo. El estado del programa
interactivo guardará la posicion central del pie que se encuentre
apoyado en un momento dado. Se arranca con un pie apoyado
mirando hacia la derecha. El pie que se dibuja sera el izquierdo si el
estado es par o el derecho si es impar. Los pies deben aparecer enteros,
no se permite una huella cortada.|#

(define POS-CENTRAL-INICIAL (/ 72 2))
(define ANCHO 900)
(define ALTO 50)
(define PASO 21)

(define
  (draw pos-central)
  (if (>= pos-central (- ANCHO 36))
      (rotate 180 (place-image IZQ pos-central (/ ALTO 2) (empty-scene ANCHO ALTO)))
      (if (even? pos-central)
          (place-image IZQ pos-central (/ ALTO 2) (empty-scene ANCHO ALTO))
          (place-image DER pos-central (/ ALTO 2) (empty-scene ANCHO ALTO)))))

(define
  (move-x x)
  (if (>= x (- ANCHO 36))
      (- PASO x)
      (+ PASO x)))

(big-bang POS-CENTRAL-INICIAL
  [to-draw draw]
  [on-tick move-x 1])











