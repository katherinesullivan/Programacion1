;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |met mont pi|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Estas primera parte nos define una función
; que genera números aleatorios
; en un intervalo real determinado.
; Recordemos que la función random nos devuelve un natural, pero para
; nuestro problema necesitamos números aleatorios fraccionarios.
(define RAND-MAX 4294967087) ; número máximo para valores aleatorios

; aleatorio : Number Number -> Number
; dados dos números a y b, devuleve un número
; aleatorio en el intervalo [a,b].
(define
  (aleatorio a b)
  (+ a           ; para que ese nro que nos dió antes esté entre a y b y no
                 ; rntre 0 y a (incluido)
     (* (- b a) ; da la cant de nros en el intervalo
                ; al mult por un nro de 0 al 1, nos da un aleat que no
                ; superará a b y sera menor o igual a a
        (/  (+ 1 (random RAND-MAX)) (* 1.0 RAND-MAX))))) ; da un nrito
                                                  ;(1 si juuuuusto coinciden)


; fin de la parte para generar números aleatorios.
; No es necesario entender cómo funciona,
; solo basta con entender el propósito de aleatorio
; (¿se acuerdan de la receta?)


; Definimos algunas constantes:
(define RADIO 1.0) ; radio del círculo
(define CENTRO (make-posn 0 0)) ; coordenadas del centro del cí­rculo

(define MAX 300000) ; cantidad de puntos a generar para nuestra estimación

; generar-puntos : Natural -> List(posn)
; Dado un natural n, devuelve una lista con n puntos aleatorios,
; con ambas componentes en el intervalo [-RADIO, RADIO].
; Es decir, dentro del cuadrado.
(define (generar-puntos n)
  (cond [(zero? n) empty]
        [else (cons (make-posn (aleatorio (- RADIO) RADIO)
                               (aleatorio (- RADIO) RADIO) )
                    (generar-puntos (- n 1)))]
        ))

; distancia : posn posn -> Number
; dados dos puntos, devuelve su distancia
(check-expect (distancia (make-posn 15 0) (make-posn 3 0)) 12)
(check-expect (distancia (make-posn 2 1) (make-posn 6 4)) 5)
(check-expect (distancia (make-posn 0 16) (make-posn 0 0)) 16)

(define (distancia p q)
  (sqrt (+ (sqr (abs (- (posn-x p) (posn-x q))))
           (sqr (abs (- (posn-y p) (posn-y q)))))))

; adentro? : posn -> Boolean
; dada una posición, determina si cae en el cí­rculo centrado en CENTRO
; y cuyo radio es RADIO
(check-expect (adentro? (make-posn 0 0)) #t)
(check-expect (adentro? (make-posn RADIO (* 2 RADIO))) #f)
(check-expect (adentro? (make-posn RADIO RADIO)) #f)

(define (adentro? p) (<= (distancia p CENTRO) RADIO)) 


; generamos una lista con muchos puntos aleatorios
(define LISTA (generar-puntos MAX))

; nos quedamos con aquellos que están dentro del cí­rculo 
(define ADENTRO (filter adentro? LISTA))

; aproximamos el área del círculo a partir de la proporción de puntos que
; caen dentro del círculo:
(define AREA (* 4.0 (/ (length ADENTRO) (length LISTA))))

; Como en nuestro caso ya sabemos cuál es el área de la superficie (pi),
; podemos usar nuestro resultado como una estimación de este número.
; Veamos quÃ© tan buena estimación es,
; calculando el error relativo porcentual
; respecto del valor que nos proporciona racket:
; calculamos el porcentaje de error:
(define ERROR (* 100 (/ (abs (- pi AREA)) pi)))


(string-append "Nuesta aproximación de pi es: "
               (number->string  (exact->inexact AREA)))

(string-append "Con un porcentaje de error de: "
               (number->string ERROR)
               "%(si lo comparamos con el valor que nos proporciona DrRacket)"
               ) 