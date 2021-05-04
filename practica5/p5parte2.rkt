;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname p5parte2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; filter : (X -> Boolean) List(X) -> List(X)

(define
  (filter1 p l)
  (cond [(empty? l) empty]
        [else (if (p (first l))
                  (cons (first l) (filter1 p (rest l)))
                  (filter1 p (rest l)))]))

(check-expect (filter1 even? (list 1 2 3 4 5))(list 2 4))
(check-expect (filter1 string? (list 3 "Lista" #true "heterogénea"))(list "Lista" "heterogénea"))

; map : (X -> Y) List(X) -> List(Y)

(define
  (map1 f l)
  (cond [(empty? l) empty]
        [else (cons (f (first l)) (map1 f (rest l)))]))

(check-expect (map1 sqr (list 1 2 3 4 5))
              (list 1 4 9 16 25))
(check-expect (map1 string-length (list "Lista" "de" "palabras"))
              (list 5 2 8))

#|La función fold recibe tres argumentos:
La función f con la que se quiere operar los elementos de la lista;
Un valor c , que es el resultado esperado para la lista vacía;
La lista l a transformar.|#

; foldr : (X X -> X) X List(X) -> X
(define
  (fold f c l)
  (cond [(empty? l) c]
        [else (f (first l) (fold f c (rest l)))]))

(check-expect (fold string-append "" (list "Pro" "gra" "ma" "ción."))
              "Programación.")
(check-expect (fold * 1 (list 1 2 3 4 5))
              120)

; Ejercicio 14

(define-struct alumno [nombre nota faltas])
; alumno (String, Number, Natural).
; Interpretación:
; - nombre representa el nombre del alumno.
; - nota representa la calificación obtenida por el alumno (entre 0 y 10).
; - faltas: número de clases a las el alumno no asistió.

#|destacados , que dada una lista de alumnos, devuelve una lista con
el nombre de aquellos alumnos que sacaron una nota mayor o igual
a 9.|#

(define
  (nota-9 alumno)
  (>= (alumno-nota alumno) 9)
  )

(define
  (nombre alumno)
  (alumno-nombre alumno))

(check-expect (destacados (list (make-alumno "Ada Lovelace" 10 20)(make-alumno "Carlos Software" 3.5 12)))
              (list "Ada Lovelace"))
; destacados: List(alumno) -> List(String)

(define
  (destacados l)
  (map nombre (filter nota-9 l))
  )

#|condicion , que dado un alumno, determine su condición de acuerdo
a las siguientes reglas:
si la nota es mayor o igual a 8, su condición es "promovido" .
Si la nota es menor a 6, su condición es "libre" .
En cualquier otro caso, la condición es "regular|#

; condicion: alumno -> String

(define
  (condicion alumno)
  (let ([nota (alumno-nota alumno)])
    (cond [(>= nota 8) "promovido"]
          [(< nota 6) "libre"]
          [else "regular"])
    )
  )


#|exito , que dada una lista de alumnos, devuelve #true si ninguno
está libre. Caso contrario, devuelve #false .|#

; exito: List(alumnos) -> Bool
(define
  (exito l)
  (cond [(empty? l) #true]
        [(cons? l) (if (< (alumno-nota (first l)) 6)
                    #false
                    (exito (rest l))
                    )]
        )
  )

(define
  (length1 l)
  (cond [(empty? l) 0]
        [(cons? l) (add1 (length (rest l)))]
        )
  )

(define
  (nota6 alumno)
  (>= (alumno-nota alumno) 6)
  )

(define
  (exito-mas-engorroso l)
  (= (length (filter nota6 l)) (length l))
  )

(check-expect (exito (list (make-alumno "Juan Computación" 5 13)
                           (make-alumno "Carlos Software" 3.5 12)
                           (make-alumno "Ada Lovelace" 10 20)))
              #false)
(check-expect (exito-mas-engorroso (list (make-alumno "Juan Computación" 5 13)
                           (make-alumno "Carlos Software" 3.5 12)
                           (make-alumno "Ada Lovelace" 10 20)))
              #false)
  