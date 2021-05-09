;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname concurso) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ejercicio 13 - Práctica 5, parte 1

#|Diseñe una función cortas que tome una lista de strings y devuelva una
lista con aquellas _palabras_ de longitud menor a 5.|#

#|

Bueno,

Primero empezamos con el diseño de datos.
 Vamos a representar palabras con strings.

; Representamos palabras con strings

Luego pasamos a la signatura.

; cortas: List(String) -> List(String)

Como nos indica el enunciado, tomamos una lista de strings y
devolvemos otra lista de palabras que como dijimos antes son representadas por strings.

Despues pasamos a la declaración de propósito, que resulta bastante parecida al enunciado. Leo

; dada una lista de strings devuelve una lista con aquellos elementos de la lista original con longitud menor a 5

Ahora, antes de la definición de la función, tenemos que pensar algunos ejemplos que después serán ejecutados
con la función check-expect para ver si nuestra función hace lo esperado.

Primero, siempre que trabajamos con listas está bueno ver qué debe suceder con la lista vacía.
Es claro que la lista vacía no tiene palabras menores a 5 pq no tiene palabras entonces escribimos
(check-expect (corta '()) '())

Después podemos ver que sucede con una lista cuyos elementos sean solamente palabras menores a 5,
en este caso, la función me debe devolver la misma lista que tomó como entrada es decir
(check-expect (corta (list "hola" "qué" "tal" "todo")) (list "hola" "qué" "tal "todo"))

También tiene sentido ver que sucede en el caso de que la lista no tenga ninguna string con longitud menor a 5.
Acá la función nos tiene que devolver la lista vacía entonces escribimos
(check-expect (corta (list "todas" "palabras" "largas")) '())

Y, por último claro, deberíamos verificar un caso donde haya tanto palabras con longitud menor a 5 como tambien mayor.
Por ejemplo, podemos hacer
(check-expect (corta (list "mezclamos" "y" "vemos" "qué" "sucede")) (list "y" "qué"))

Entonces, hechos estos pasos previos, podemos ahora sí pasar a la definición de la función propiamente dicha

Cómo podríamos pensar el funcionamiento de la misma?
En definitiva lo que tenemos qué hacer es a partir de nuestra lista pasada como argumento,  ver cuales de todos sus elementos
cumplen la condición de tener una longitud menor a 5.
Pensandolo así, lo que podemos hacer es de alguna manera ir recorriendo la lista y fijándonos elemento por elemento cuáles
son los que cumplen con la condición e ir agregándolos a otra lista.

Pero bueno, ¿cómo hacemos esto en Racket?

Sabemos que tenemos acceso al primer elemento de la lista mediante la función first.
 Pero, como accedemos al resto de los elementos?
Además de first para las listas en Racket contamos con la función rest que nos devuelve la lista sin su primer elemento,
entonces,
una manera de acceder al segundo elemento es aplicando first sobre el resultado de hacer rest de la lista
una manera de acceder al tercer elemento es aplicando first sobre el resultado de hacer rest a el resultado
de haber hecho rest a la lista original
y así sucesivamente

asi podemos ir accediendo a cada uno de los elementos y comprobar si cumplen con la condicion

pero, la pregumta ahora es ¿como vamos a ir guardando esos elementos en una lista?

un abordaje logico puede ser:
una vez que tengo un elemento que cumple la condicion hago uso del constructor cons para agregarlo a una
lista formada por todos los elementos que cumplan la condición que vengan después de él en la lista.

Por lo que dijimos antes sé como ir accediendo al segundo elemento de la lista y luego al tercero cuarto etc -> mediante rest
y casualmente una función que me permite encontrar los strings cuya longitud sea menor a 5 en el rest de la lista es
la misma función que estamos definiendo (esto pq rest nos devuelve una lista). Vamos a usar recursión es decir,
la función se va a llamar a sí misma.

Entonces, ahí estaríamos bastante cerca de terminar de definir nuestra función: 
lo que vamos a hacer es

// escribir 
chequeamos si el primer elemento cumpla la condición
si lo hace vamos a hacer (cons (first l) (cortas (rest l)))
-bueno aca suponemos que nuestra lista se llama l-
si no la cumple simplemente queremos ver si las strings que siguen la cumplen, o sea vamos a simplemente devolver
si no la cumple (cortas (rest l))

Pero bueno acá nos falta algo, 
pensemos si nuestra función fuese así y la llamo con una lista de 2  hola y chau llamo a la funcion cumple con la condicion asi
que hace cons... se llama cortas con rest de l y tambien hacemos cons... pero luego rest de l es una lista vacia y cuando llamem
a la funcion con la lista vacía al querer hacer first esto resultará en un error.

no solo que ya estaría mal que la función no esté definida para la lista vacía (pues es una entrada que nos pueden pasar) sino
que tambien hay q tener en cuenta q la recursion siempre necesita de un caso base
en este caso, la lista vacia

entonces a lo que ya teníamos escrito le vamos a agregar el lo que sucede si tenemos una lista vacia
si la lista es vacia devolvemos la lista vacia
si no lo es
chequeamos...

Ahora solo queda escribir esto en Racket

(define
(cortas l)
(cond [(empty? l) '()]
      [(cons? l)]
      si aca tenemos un else para ver si nos pasaron algo disntinto a una lista usariamos el cons? pero como en este caso
en el tipado o signatura declaramos que esta funcion solo recibira listas de strings no existira otra posibilidad asi que
escribimos simplemente
)

(define
(cortas l)
(cond [(empty? l) '()]
      [if (long-menor-a-5 (first l))
           (cons (first l) (cortas l))
           (cortas l)])
)

y bueno queda como último detalle definir la funcion con la condicion

Si quieren podemos poner en el diseño de datos
;Representamos palabras con strings

La signatura quedaría
;long-menor-a-5: String -> Bool
Y el propósito de la función
; devuelve true si la palabra tiene una longitud menor a 5 y false en caso contrario

Ponemos unos check-expect
de algo que sepamos que nos de true
(check-expect (long-menor-a-5 "hola") #t)
de algo que sepamos que nos va a dar false
(check-expect (long-menor-a-5 "palabra") #f)
y de algo interesante por ejemplo la lista vacia
su longitud es 0 por lo tanto nos va a dar true
(check-expect (long-menor-a-5 "") #t)

y definimos

(define
(long-menor-a-5 s)
en una primer instancia esto lo podemos definir con if
(if (< (string-length s) 5) #t #f)
)

pero como esencialmente devolvemos #t si la condicion es true y #f si la condicion es false
la funcion entera directamente puede ser la condicion es decir que nos queda

(define
(long-menor-a-5 s)
(< (string-length s) 5)
)

Cabe aclarar que esto despues lo van a poder hacer con filter



(define
(corta)
)

Corta como dijimos toma una lista, llamemosla l

(define
(corta l)
)





|#

(string-length "")

; corta? : String -> Bool
; dado un string me va a devolver #true si este tiene longitud menor a 5 y el string consta de una sola
; palabra y #false en caso contrario

(check-expect (pal-corta? "palabra") #f)
(check-expect (pal-corta? "hey") #t)
(check-expect (pal-corta? "") #t)
(check-expect (pal-corta? "a a") #f)

(define
  (pal-corta? s)
  (and (< (string-length s) 5) (false? (member? #\space (string->list s))))
  )

; aclarar que los que tengan mas de una palabra no va


; cortas: List(String) -> List(String)
; dada una lista de strings devuelve una lista con aquellas palabras con longitud menor a 5

(check-expect (cortas '()) '())
(check-expect (cortas (list "holas" "queeee" "tallll")) '())
(check-expect (cortas (list "hola" "antiparras" "comunismo")) (list "hola"))
(check-expect (cortas (list "hola" "que" "tal")) (list "hola" "que" "tal"))
(check-expect (cortas 5) "Tipo de dato ingresado inválido")
(check-expect (cortas (list "oh f" "rayo" "australia")) (list "rayo"))

(define
  (cortas l)
  (cond [(empty? l) '()]
        [(cons? l) (if (pal-corta? (first l))
                   (cons (first l) (cortas (rest l)))
                   (cortas (rest l)))]
        [else "Tipo de dato ingresado inválido"]
        )
  )

; aclarar lo de filter 