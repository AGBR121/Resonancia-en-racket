#lang racket

#|
- Fecha:  05/05/2024
- Hora de Publicacion: 8:20pm
- Version del código: 2.0
- Autor: Ing(c) Burbano Rodriguez Angel Gabriel
- Lenguaje utilizado: Racket
- Versión del lenguaje: 8.8
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Descripcion del programa: Este programa consiste en un algoritmo para analizar la resonancia magnetica de un paciente
dada en un vector o en string, la gama de colores es una de grises el cual, va de 0 a 255, en este programa,
se escogera aleatoriamente los "colores" desde el color 20 al color 42 y será utilizados de acuerdo a la necesidad
del cliente.
- SALVEDAD: El programa funciona con valores del 1 al 100 (que son los valores que una persona normal/medico usaria).
|#

;Llamamos la libreria graphics
(require graphics/graphics)
;Abrimos los graficos
(open-graphics)

#|--------------------------------------------------------------------------
Funcion Cerebro que obtiene el string de una pagina de la resonancia
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local z: Es el que detiene el ciclo cuando se genere el string de la pagina
- Identificador local y: Es el que cuenta la cantida de valores en el eje y
- Identificador local x: Es el que cuenta la cantida de valores en el eje x
|#
(define (Cerebro str z y x)
;Identificador local randomColor que escoje aleatoriamente un numero del 0 al 255
(define randomColor (integer->char(random 20 45))) 
  (if (<= z 1)
          (if (<= y 100)
              (if (<= x 100)
                  (Cerebro (string-append str (~a randomColor)) z y (+ 1 x))
              ;De lo contrario
                  (Cerebro str z (+ y 1) 1)
              );Fin if (<= x 100)
         ;De lo contrario     
         (Cerebro str (+ z 1) 1 1)
      );Fin if (<= y 100)
  ;De lo contrario
      str
  );Fin if (<= z 1)
);Fin función Cerebro

#|--------------------------------------------------------------------------
Funcion CicloCerebro que obtiene el string de toda la resonancia
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local counter: Es el que cuenta hasta que se obtenga las 100 paginas
|#

(define (CicloCerebro str counter)
 (if (<= counter 100)
     (CicloCerebro (string-append str (Cerebro "" 1 1 1 )) (+ counter 1))
 ;De lo contrario
     str
 );Fin if (<= counter 20)
);Fin función CicloCerebro

;Identificador de primera clase stringCerebro que guarda la cadena de string de la resonancia
(define stringCerebro (CicloCerebro "" 1))

#|--------------------------------------------------------------------------
Funcion Cerebro2 que obtiene el vector de una pagina de la resonancia
- Identificador local vect: Guarda el vector de la funcion
- Identificador local z: Es el que detiene el ciclo cuando se genere el string de la pagina
- Identificador local y: Es el que cuenta la cantida de valores en el eje y
- Identificador local x: Es el que cuenta la cantida de valores en el eje x
|#
(define (Cerebro2 vect z y x)
;Identificador local randomColor que escoje aleatoriamente un numero del 0 al 255
(define randomColor (random 20 44)) 
  (if (<= z 1)
          (if (<= y 100)
              (if (<= x 100)
                  (Cerebro2 (vector-append vect (make-vector 1 randomColor )) z y (+ 1 x))
              ;De lo contrario
                  (Cerebro2 vect z (+ y 1) 1)
              );Fin if (<= x 100)
         ;De lo contrario     
         (Cerebro2 vect (+ z 1) 1 1)
      );Fin if (<= y 100)
  ;De lo contrario
      vect
  );Fin if (<= z 1)
);Fin función Cerebro2

#|--------------------------------------------------------------------------
Funcion CicloCerebro2 que obtiene el vector de toda la resonancia
- Identificador local vec: Guarda el vector de la funcion
- Identificador local counter: Es el que cuenta hasta que se obtenga las 100 paginas
|#

(define (CicloCerebro2 vec counter)
 (if (<= counter 100)
     (CicloCerebro2 (vector-append vec (Cerebro2 (vector) 1 1 1 )) (+ counter 1))
 ;De lo contrario
     vec
 );Fin if (<= counter 20)
);Fin función CicloCerebro

;Identificador de primera clase stringCerebro que guarda la cadena de string de la resonancia
(define vectorCerebro (CicloCerebro2 (vector) 1))

;Funcion String que guarda toda la resonancia en formato string
(define (String)


#|--------------------------------------------------------------------------
Funcion PuntoSospechoso que verifica que el punto dado es un punto sospechoso o no
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
|#
(define (PuntoSospechoso str x y z)
(if (and
              ;Pagina Anterior
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) ))
              ;Pagina Ubicada
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 z)) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 z)) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 z)) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 z)) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 z)) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 z)) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 z)) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 z)) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 z)) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 z)) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 z)) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 z)) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 z)) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 z)) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 z)) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 z)) (* 100 (+ y 1))) ) ))
              ;Pagina Siguiente
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (+ z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (+ z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (+ z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (+ z 1))) (* 100 y)) ) ))
         )
     #t
    ;De lo contrario
     #f
    )
)
;Funcion TotalPuntosSospechosos que realiza la funcion principal de los puntos sospechosos
(define (TotalPuntosSospechosos)
#|--------------------------------------------------------------------------
Funcion AbrirGrafico1 que permite realizar la funcion para mostrar los resultados del punto recibido y mostrar si este
punto es sospechoso o no
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local counter: Es el que cuenta hasta que se obtenga los puntos de la pagina anterior, la pagina actual
y la pagina siguiente
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay puntos sospechosos alrededor del punto ubicado de la pagina
|#
(define (AbrirGrafico1 str x y z counter counterSospechoso)
;Abrimos la ventana PuntoSospechoso que mostrará los resultados
(define PuntoSospechoso (open-viewport "Puntos Sospechoso" 1100 450))

#|--------------------------------------------------------------------------
Funcion DibujarCuadros que dibujará en la ventana de PuntoSospechoso los resultados del punto recibido
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local counter: Es el que cuenta hasta que se obtenga los puntos de la pagina anterior, la pagina actual
y la pagina siguiente
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay puntos sospechosos alrededor del punto ubicado de la pagina
|#
(define (DibujarCuadros str x y z counter counterSospechoso)

 (if (= counter 1)
  [begin
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 130) (~a y ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 180) (~a (+ y 1) ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 230) (~a (+ y 2) ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 90) (~a x ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 90) (~a (+ x 1) ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 90) (~a (+ x 2) ))
  ];Fin begin
 ;De lo contrario
  [begin
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 130) "Y-1")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 180) "Y")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 230) "Y+1")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 90) "X-1")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 90) "X")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 90) "X+1")
  ];Fin begin
 );Fin if (= counter 1)
 (if (< counter 3)
    [begin
     ;Dibuja los cuadrados donde se colocan los datos
     ((draw-rectangle PuntoSospechoso) (make-posn (+ (* counter 350) 100) 100) 150 150) ;Cuadrado grande
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 150) 100) (make-posn (+ (* counter 350) 150) 249)) ;linea 1 arriba abajo
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 200) 100) (make-posn (+ (* counter 350) 200) 249)) ;linea 2 arriba abajo
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 100) 150) (make-posn (+ (* counter 350) 249) 150)) ;Linea 1 izquierda derecha
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 100) 200) (make-posn (+ (* counter 350) 249) 200)) ;Linea 2 izquierda derecha
     ;Dibuja la flecha
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 50) (make-posn (+ (* counter 350) 300) 50)) ;Linea x
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 50) (make-posn (+ (* counter 350) 50) 300)) ;Linea y
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 300) (make-posn (+ (* counter 350) 60) 290))
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 300) (make-posn (+ (* counter 350) 40) 290))
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 300) 50) (make-posn (+ (* counter 350) 290) 40))
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 300) 50) (make-posn (+ (* counter 350) 290) 60))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 310) 55) "X")
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 45) 320) "Y")
     ;Dibuja los datos
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 130) (~a ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 180) (~a ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y) ) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 230) (~a ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 130) (~a ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) )))
     (if (= counter 1)
         ((draw-solid-ellipse PuntoSospechoso) (make-posn (+ (* counter 350) 151) 151) 50 50 "Firebrick")
     ;De lo contrario
         ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 180) (~a ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 y) ) ) )))
     );Fin if (= counter 1)
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 230) (~a ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 130) (~a ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 180) (~a ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y) ) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 230) (~a ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 100) 275) "Plano en Z (hoja) =")
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 225) 275) (~a z))
     (if (= counter 0)
         ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 100) 300) "Plano inferior")
     ;De lo contrario
         (if (= counter 2)
             ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 100) 300) "Plano superior")
         ;De lo contrario
             (void)
         );Fin if (= counter 2)
     );Fin if (= counter 0)
     (if (and (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
              (if ( not (= counter 1) )
                  [begin
                    (if (and (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) ) )
                        (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) ))
                        )
                        #t
                    ;De lo contrario
                        #f
                    )
                  ];Fin begin
              ;De lo contrario
                  #t
              );Fin if ( not (= counter 1) )
              (<= 20 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) ))
              (<= 20 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) )
              (>= 40 ( char->integer (string-ref str ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
         )
     (DibujarCuadros str x y (+ z 1) (+ counter 1) (+ counterSospechoso 1))
    ;De lo contrario
     (DibujarCuadros str x y (+ z 1) (+ counter 1) counterSospechoso )
    );Fin if 
   ];Fin begin
 ;De lo contrario
    (if (= counterSospechoso 3)
        ((draw-string PuntoSospechoso) (make-posn 300 400) "Este punto ES SOSPECHOSO")
    ;De lo contrario
        ((draw-string PuntoSospechoso) (make-posn 300 400) "Este punto NO ES SOSPECHOSO")
    );Fin if (= counterSospechoso 3)
 );Fin if (< counter 3)
);Fin funcion DibujarCuadros
;Llamamos la función que dibuja los cuadros 
(DibujarCuadros str x y z counter counterSospechoso)
);Fin función AbrirGrafico1

(printf "Vamos a revisar si el punto que nos dará es un punto sospechoso o no.\n")

;Funcion x1 que guarda el valor del eje x para el primer gráfico
(define (x1)
(printf "\nIngrese valor del eje x: ")
;Identificador local x que recibe el valor ingresado por teclado para el eje x
(define x (- (read) 1) )
  (if (or (<= x 0) (>= x 99))
      [begin
        (printf "\nNo se puede tomar el punto ")
        (x1)
      ];Fin begin
  ;De lo contrario
      x
  );Fin if (or (= x 0) (= x 99))
);Fin funcion x1

;Funcion y1 que guarda el valor del eje y para el primer gráfico
(define (y1)
(printf "\nIngrese valor del eje y: ")
;Identificador local y que recibe el valor ingresado por teclado para el eje y
(define y (- (read) 1) )
  (if (or (<= y 0) (>= y 99))
      [begin
        (printf "\nNo se puede tomar el punto ")
        (y1)
      ];Fin begin
  ;De lo contrario
      y
  );Fin if (or (= y 0) (= y 99)) 
);Fin función y1

;Funcion z1 que guarda el valor del eje z para el primer gráfico
(define (z1)
(printf "\nIngrese valor del eje z: ")
;Identificador local z que recibe el valor ingresado por teclado para el eje z
(define z (- (read) 1) )
  (if (or (<= z 0) (>= z 99))
      [begin
        (printf "\nNo se puede tomar el punto ")
        (z1)
      ];Fin begin
  ;De lo contrario
      z
  );Fin if (or (= z 0) (= z 99))
);Fin funcion z1

;Hacemos llamado a la función AbrirGrafico1
(AbrirGrafico1 stringCerebro (x1) (y1) (z1) 0 0)
);Fin funcion TotalPuntosSospechosos

;Funcion TotalLineasSospechosas que permite realizar la funcion principal del grafico de las lineas sospechosas
(define (TotalLineasSospechosas)


#|--------------------------------------------------------------------------
Funcion AbrirGrafico2 que permite realizar la funcion para mostrar los resultados de la pagina recibada y mostrar las
lineas sospechosas que presenta
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay 3 o más puntos sospechosos seguidos
|#
(define (AbrirGrafico2 str  x y z counterSospechoso)

;Abrimos la gráfica/lienzo
(define LineasSospechosa (open-viewport (string-append "Lineas sospechosas para el plano " (~a (+ z 1)) ) 948 948))
;Insertamos la plantilla para ubicar las lineas sospechosas
(define image "PlantillaLineas.JPG")  
(((draw-pixmap-posn image ) LineasSospechosa) (make-posn 30 30))
((draw-line LineasSospechosa) (make-posn 15 0) (make-posn 15 913) "red") 
((draw-line LineasSospechosa) (make-posn 0 15) (make-posn 913 15) "red")
((draw-line LineasSospechosa) (make-posn 15 913) (make-posn 0 903) "red") 
((draw-line LineasSospechosa) (make-posn 15 913) (make-posn 30 903) "red")
((draw-line LineasSospechosa) (make-posn 913 15) (make-posn 903 0) "red")
((draw-line LineasSospechosa) (make-posn 913 15) (make-posn 903 30) "red")
((draw-string LineasSospechosa) (make-posn 918 22) "X" "red")
((draw-string LineasSospechosa) (make-posn 11 934) "Y" "red")

#|--------------------------------------------------------------------------
Funcion LineasSospechosas que dibuja en el lienzo, los datos del grafico 2.
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay 3 o más puntos sospechosos seguidos
|#  
(define (LineasSospechosas str  x y z counterSospechoso)
  
(if (>= counterSospechoso 3)
    [begin
      ((draw-solid-rectangle LineasSospechosa)(make-posn (+(* 9 (- x 1)) 48) (+(* 9 y) 48)) 8 8 "ForestGreen") 
      ((draw-solid-rectangle LineasSospechosa)(make-posn (+(* 9 (- x 2)) 48) (+(* 9 y) 48)) 8 8 "ForestGreen")
      ((draw-solid-rectangle LineasSospechosa)(make-posn (+(* 9 (- x 3)) 48) (+(* 9 y) 48)) 8 8 "ForestGreen")
      ];Fin begin
;De lo contrario
    (void)
);Fin if (>= counterSospechoso 3)
  
(if (< y 99 )
    (if (< x 99 )
        (if (PuntoSospechoso str x y z)
              (LineasSospechosas str  (+ x 1) y z (+ counterSospechoso 1) )
        ;De lo contrario
              (LineasSospechosas str  (+ x 1) y z 0)
        );Fin if (PuntoSospechoso str x y z)
     ;De lo contrario
        (LineasSospechosas str  1 (+ y 1) z 0)
     );Fin if (< x 99 )
  ;De lo contrario
     (void)
  );Fin if (< Y 99 )
);Fin funcion LineasSospechosas
;Llamamos la funcion
(LineasSospechosas str x y z counterSospechoso)
);Fin funcion AbrirGrafico2


;Funcion Todas que dibuja las 98 ventanas de la resonancia
(define (Todas counter)
(if (< counter 99)
 [begin
   ;Llamamos la función AbrirGrafico2
   (AbrirGrafico2 stringCerebro 1 1 counter 0)
   (Todas (+ counter 1))
 ];Fin begin
 (void)
);Fin if (< counter 99)
);Fin funcion Todas
;Llamamos la funcion Todas  
(Todas 1)
);Fin funcion TotalLineasSospechosas

;FuncionInformeResonancia
(define (InformeResonancia)
#|--------------------------------------------------------------------------
Funcion Informe que Imrpime el informe final de toda la resonancia.
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterPuntosSospechosos: que cuenta la cantidad de puntos sospechosos
- Identificador local counterPuntosSeguidos: que cuenta la cantidad de puntos sospechosos seguidos
- Identificador local counterLineas: que cuenta la cantidad de lineas sospechosas
|#  
(define (Informe str x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
(newline)
(printf "---------------------------------------------------------------------------------------------------\n")
(printf "                                        INFORME FINAL                                              \n")
(printf "---------------------------------------------------------------------------------------------------\n")
(printf " Plano en Z(Fotografía)  | Líneas sospechosas x plano(foto) | Puntos sospechosos por plano(foto) \n")
(printf "---------------------------------------------------------------------------------------------------\n")
#|--------------------------------------------------------------------------
Funcion local InformeUnaPagina que Imrpime el informe final de una pagina de la resonancia.
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterPuntosSospechosos: que cuenta la cantidad de puntos sospechosos
- Identificador local counterPuntosSeguidos: que cuenta la cantidad de puntos sospechosos seguidos
- Identificador local counterLineas: que cuenta la cantidad de lineas sospechosas
|#  
(define (InformeUnaPagina str x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
(if (= counterPuntosSeguidos 3)
    (if (< y 99 )
        (if (< x 99 )
            (if (PuntoSospechoso str x y z)
                (InformeUnaPagina str (+ 1 x) y z (+ 1 counterPuntosSospechosos) (+ 1 counterPuntosSeguidos) (+ counterLineas 1))
            ;De lo contrario
                (InformeUnaPagina str (+ 1 x) y z counterPuntosSospechosos 0 (+ counterLineas 1))
            );Fin if (PuntoSospechoso str x y z)
       ;De lo contrario
            (InformeUnaPagina str 1 (+ y 1) z counterPuntosSospechosos 0 counterLineas)
       );Fin if (< x 99 )
   ;De lo contrario
       [begin
         ( if (= (quotient (+ z 1) 10) 0)
              [begin     
                (printf (string-append "             ~a           |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a                \n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         ;De lo contrario
              [begin     
                (printf (string-append "             ~a          |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a                \n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         );Fin if (= (quotient (+ z 1) 10) 0)
       ];Fin begin
   );Fin if (< y 99 )
   (if (< y 99 )
       (if (< x 99 )
           (if (PuntoSospechoso str x y z)
               (InformeUnaPagina str (+ 1 x) y z (+ 1 counterPuntosSospechosos) (+ 1 counterPuntosSeguidos) counterLineas)
           ;De lo contrario
               (InformeUnaPagina str (+ 1 x) y z counterPuntosSospechosos 0 counterLineas)
           );Fin if (PuntoSospechoso str x y z)
       ;De lo contrario
           (InformeUnaPagina str 1 (+ y 1) z counterPuntosSospechosos 0 counterLineas)
       );Fin if (< x 99 )
       [begin
         ( if (= (quotient (+ z 1) 10) 0)
              [begin     
                (printf (string-append "             ~a           |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a                \n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         ;De lo contrario
              [begin     
                (printf (string-append "             ~a          |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a                \n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         );Fin if (= (quotient (+ z 1) 10) 0)
       ];Fin begin
  );Fin if (< y 99 )
);Fin if (= counterPuntosSeguidos 3)
)

 #|--------------------------------------------------------------------------
Funcion local InformeCompleto que Imrpime el informe final de todas las paginas de la resonancia de manera recursiva.
- Identificador local str: Guarda la cadena de string de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterPuntosSospechosos: que cuenta la cantidad de puntos sospechosos
- Identificador local counterPuntosSeguidos: que cuenta la cantidad de puntos sospechosos seguidos
- Identificador local counterLineas: que cuenta la cantidad de lineas sospechosas
|#   
(define (InformeCompleto str x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
(if (<= z 98)
    [begin
      (InformeUnaPagina str x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
      (InformeCompleto str x y (+ z 1) counterPuntosSospechosos counterPuntosSeguidos counterLineas)
    ];Fin befin
;De lo contrario
    (void)
);Fin if (<= z 98)
);Fin funcion InformeCompleto
;Llamamos la funcion InformeCompleto
(InformeCompleto str x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
);Fin funcion Informe
;Llamamos la funcion Informe
(Informe stringCerebro 1 1 1 0 0 0)
);Fin funcion InformeResonancia

;Llamamos al menú de la funcion
(define (menu)
(printf "\nBienvenido! Porfavor digita el número de la opción a usar:\n")
(printf "1- Analizar punto (x, y, z)\n")
(printf "2- Analizar plano (z)\n")
(printf "3- Mostrar informe\n")
(printf "4- Volver atrás\n")

;Funcion respuesta1 que permite digitar y evaluar la respuesta
(define (respuesta1)
;Identificador option que recibe el valor digitado or el usuario por teclado
(define option (read))
    (if (= option 1)
        [begin
        (TotalPuntosSospechosos)
        (menu)
        ]
    ;De lo contrario
        (if (= option 2)
            [begin
            (TotalLineasSospechosas)
            (menu)
            ]
        ;De lo contrario
            (if (= option 3)
                [begin
                (InformeResonancia)
                (menu)
                ]
            ;De lo contrario
                (if (= option 4)
                    (Menu)
                ;De lo contrario
                    [begin
                      (printf "\nNo es valida la respuesta, escriba otro número: ")
                      (respuesta1)
                    ]
                );Fin if (= option 4)   
            );Fin if (= option 3)
        );Fin if (= option 2)
    );Fin if (= option 1)
);Fin funcion respuesta1
;Llamamos la funcion respuesta1
(respuesta1)
  )
 (menu)
);Fin funcion string

;Funcion Vector que guarda toda la resonancia en formato vector
(define (Vector)


#|--------------------------------------------------------------------------
Funcion PuntoSospechoso que verifica que el punto dado es un punto sospechoso o no
- Identificador local vect: Guarda el vector de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
|#
(define (PuntoSospechoso vect x y z)
(if (and
              ;Pagina Anterior
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) )
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) )
              ;Pagina Ubicada
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 z)) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 z)) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 z)) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 z)) (* 100 y)) ) )
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 z)) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 z)) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 z)) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 z)) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 z)) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 z)) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 z)) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 z)) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 z)) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 z)) (* 100 y)) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 z)) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 z)) (* 100 (+ y 1))) ) )
              ;Pagina Siguiente
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 y)) ) )
              (<= 20 ( vector-ref vect ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (- x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 (+ z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 (+ z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 (+ z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 y)) ) )
              (<= 20 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vect ( + (+ (+ x 1) (* 10000 (+ z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vect ( + (+ x (* 10000 (+ z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vect ( + (+ x (* 10000 (+ z 1))) (* 100 y)) ) )
         )
     #t
    ;De lo contrario
     #f
    )
)
;Funcion TotalPuntosSospechosos que realiza la funcion principal de los puntos sospechosos
(define (TotalPuntosSospechosos)
#|--------------------------------------------------------------------------
Funcion local AbrirGrafico1 que permite realizar la funcion para mostrar los resultados del punto recibido y mostrar si este
punto es sospechoso o no
- Identificador local vect: Guarda el vector de la funcion
- Identificador local counter: Es el que cuenta hasta que se obtenga los puntos de la pagina anterior, la pagina actual
y la pagina siguiente
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay puntos sospechosos alrededor del punto ubicado de la pagina
|#
(define (AbrirGrafico1 vect x y z counter counterSospechoso)
;Abrimos la ventana PuntoSospechoso que mostrará los resultados
(define PuntoSospechoso (open-viewport "Puntos Sospechoso" 1100 450))

#|--------------------------------------------------------------------------
Funcion local DibujarCuadros que dibujará en la ventana de PuntoSospechoso los resultados del punto recibido
- Identificador local vec: Guarda el vector de la funcion
- Identificador local counter: Es el que cuenta hasta que se obtenga los puntos de la pagina anterior, la pagina actual
y la pagina siguiente
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay puntos sospechosos alrededor del punto ubicado de la pagina
|#
(define (DibujarCuadros vec x y z counter counterSospechoso)

 (if (= counter 1)
  [begin
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 130) (~a y ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 180) (~a (+ y 1) ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 230) (~a (+ y 2) ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 90) (~a x ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 90) (~a (+ x 1) ))
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 90) (~a (+ x 2) ))
  ];Fin begin
 ;De lo contrario
  [begin
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 130) "Y-1")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 180) "Y")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 70) 230) "Y+1")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 90) "X-1")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 90) "X")
    ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 90) "X+1")
  ];Fin begin
 );Fin if (= counter 1)
 (if (< counter 3)
    [begin
     ;Dibuja los cuadrados donde se colocan los datos
     ((draw-rectangle PuntoSospechoso) (make-posn (+ (* counter 350) 100) 100) 150 150) ;Cuadrado grande
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 150) 100) (make-posn (+ (* counter 350) 150) 249)) ;linea 1 arriba abajo
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 200) 100) (make-posn (+ (* counter 350) 200) 249)) ;linea 2 arriba abajo
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 100) 150) (make-posn (+ (* counter 350) 249) 150)) ;Linea 1 izquierda derecha
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 100) 200) (make-posn (+ (* counter 350) 249) 200)) ;Linea 2 izquierda derecha
     ;Dibuja la flecha
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 50) (make-posn (+ (* counter 350) 300) 50)) ;Linea x
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 50) (make-posn (+ (* counter 350) 50) 300)) ;Linea y
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 300) (make-posn (+ (* counter 350) 60) 290))
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 50) 300) (make-posn (+ (* counter 350) 40) 290))
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 300) 50) (make-posn (+ (* counter 350) 290) 40))
     ((draw-line PuntoSospechoso) (make-posn (+ (* counter 350) 300) 50) (make-posn (+ (* counter 350) 290) 60))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 310) 55) "X")
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 45) 320) "Y")
     ;Dibuja los datos
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 130) (~a (vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 180) (~a (vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y) ) ) ))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 120) 230) (~a (vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1) )) )))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 130) (~a (vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ))
     (if (= counter 1)
         ((draw-solid-ellipse PuntoSospechoso) (make-posn (+ (* counter 350) 151) 151) 50 50 "Firebrick")
     ;De lo contrario
         ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 180) (~a (vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 y) ) ) ))
     );Fin if (= counter 1)
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 170) 230) (~a (vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 130) (~a (vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1))))))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 180) (~a (vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y) ) ) ))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 220) 230) (~a (vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1))))))
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 100) 275) "Plano en Z (hoja) =")
     ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 225) 275) (~a z))
     (if (= counter 0)
         ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 100) 300) "Plano inferior")
     ;De lo contrario
         (if (= counter 2)
             ((draw-string PuntoSospechoso) (make-posn (+ (* counter 350) 100) 300) "Plano superior")
         ;De lo contrario
             (void)
         );Fin if (= counter 2)
     );Fin if (= counter 0)
     (if (and (<= 20 ( vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (- y 1))) ) )
              (<= 20 ( vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) ) 
              (>= 40 ( vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 y)) ) )
              (<= 20 ( vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1)) ) ) )
              (>= 40 ( vector-ref vec ( + (+ (- x 1) (* 10000 (- z 1))) (* 100 (+ y 1)))))
              (<= 20 ( vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) ) 
              (>= 40 ( vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 (- y 1))) ) )
              (if ( not (= counter 1) )
                  [begin
                    (if (and (<= 20 ( vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) ) 
                        (>= 40 ( vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 y)) ) )
                        )
                        #t
                    ;De lo contrario
                        #f
                    )
                  ];Fin begin
              ;De lo contrario
                  #t
              );Fin if ( not (= counter 1) )
              (<= 20 ( vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) ) 
              (>= 40 ( vector-ref vec ( + (+ x (* 10000 (- z 1))) (* 100 (+ y 1))) ) )
              (<= 20 ( vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1)) ) ) )
              (>= 40 ( vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (- y 1)) ) ))
              (<= 20 ( vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ) )
              (>= 40 ( vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 y)) ))
              (<= 20 ( vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1)) ) ) )
              (>= 40 ( vector-ref vec ( + (+ (+ x 1) (* 10000 (- z 1))) (* 100 (+ y 1)) ) ))
         )
     (DibujarCuadros vec x y (+ z 1) (+ counter 1) (+ counterSospechoso 1))
    ;De lo contrario
     (DibujarCuadros vec x y (+ z 1) (+ counter 1) counterSospechoso )
    );Fin if 
   ];Fin begin
 ;De lo contrario
    (if (= counterSospechoso 3)
        ((draw-string PuntoSospechoso) (make-posn 300 400) "Este punto ES SOSPECHOSO")
    ;De lo contrario
        ((draw-string PuntoSospechoso) (make-posn 300 400) "Este punto NO ES SOSPECHOSO")
    );Fin if (= counterSospechoso 3)
 );Fin if (< counter 3)
);Fin funcion DibujarCuadros
;Llamamos la función que dibuja los cuadros 
(DibujarCuadros vect x y z counter counterSospechoso)
);Fin función AbrirGrafico1

(printf "Vamos a revisar si el punto que nos dará es un punto sospechoso o no.\n")

;Funcion x1 que guarda el valor del eje x para el primer gráfico
(define (x1)
(printf "\nIngrese valor del eje x: ")
;Identificador local x que recibe el valor ingresado por teclado para el eje x
(define x (- (read) 1) )
  (if (or (<= x 0) (>= x 99))
      [begin
        (printf "\nNo se puede tomar el punto ")
        (x1)
      ];Fin begin
  ;De lo contrario
      x
  );Fin if (or (= x 0) (= x 99))
);Fin funcion x1

;Funcion y1 que guarda el valor del eje y para el primer gráfico
(define (y1)
(printf "\nIngrese valor del eje y: ")
;Identificador local y que recibe el valor ingresado por teclado para el eje y
(define y (- (read) 1) )
  (if (or (<= y 0) (>= y 99))
      [begin
        (printf "\nNo se puede tomar el punto ")
        (y1)
      ];Fin begin
  ;De lo contrario
      y
  );Fin if (or (= y 0) (= y 99)) 
);Fin función y1

;Funcion z1 que guarda el valor del eje z para el primer gráfico
(define (z1)
(printf "\nIngrese valor del eje z: ")
;Identificador local z que recibe el valor ingresado por teclado para el eje z
(define z (- (read) 1) )
  (if (or (<= z 0) (>= z 99))
      [begin
        (printf "\nNo se puede tomar el punto ")
        (z1)
      ];Fin begin
  ;De lo contrario
      z
  );Fin if (or (= z 0) (= z 99))
);Fin funcion z1

;Hacemos llamado a la función AbrirGrafico1
(AbrirGrafico1 vectorCerebro (x1) (y1) (z1) 0 0)
);Fin funcion TotalPuntosSospechosos

;Funcion TotalLineasSospechosas que permite realizar la funcion principal del grafico de las lineas sospechosas
(define (TotalLineasSospechosas)

#|--------------------------------------------------------------------------
Funcion AbrirGrafico2 que permite realizar la funcion para mostrar los resultados de la pagina recibada y mostrar las
lineas sospechosas que presenta.
- Identificador local vect: Guarda el vector de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay 3 o más puntos sospechosos seguidos
|#
(define (AbrirGrafico2 vect  x y z counterSospechoso)

;Abrimos la gráfica/lienzo
(define LineasSospechosa (open-viewport (string-append "Lineas sospechosas para el plano " (~a (+ z 1)) ) 948 948))
;Insertamos la plantilla para ubicar las lineas sospechosas
(define image "PlantillaLineas.JPG")  
(((draw-pixmap-posn image ) LineasSospechosa) (make-posn 30 30))
((draw-line LineasSospechosa) (make-posn 15 0) (make-posn 15 913) "red") 
((draw-line LineasSospechosa) (make-posn 0 15) (make-posn 913 15) "red")
((draw-line LineasSospechosa) (make-posn 15 913) (make-posn 0 903) "red") 
((draw-line LineasSospechosa) (make-posn 15 913) (make-posn 30 903) "red")
((draw-line LineasSospechosa) (make-posn 913 15) (make-posn 903 0) "red")
((draw-line LineasSospechosa) (make-posn 913 15) (make-posn 903 30) "red")
((draw-string LineasSospechosa) (make-posn 918 22) "X" "red")
((draw-string LineasSospechosa) (make-posn 11 934) "Y" "red")

#|--------------------------------------------------------------------------
Funcion LineasSospechosas que dibuja en el lienzo, los datos del grafico 2.
- Identificador local vec: Guarda el vector de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterSospechoso: que cuenta si hay 3 o más puntos sospechosos seguidos
|#  
(define (LineasSospechosas vec  x y z counterSospechoso)
  
(if (>= counterSospechoso 3)
    [begin
      ((draw-solid-rectangle LineasSospechosa)(make-posn (+(* 9 (- x 1)) 48) (+(* 9 y) 48)) 8 8 "ForestGreen") 
      ((draw-solid-rectangle LineasSospechosa)(make-posn (+(* 9 (- x 2)) 48) (+(* 9 y) 48)) 8 8 "ForestGreen")
      ((draw-solid-rectangle LineasSospechosa)(make-posn (+(* 9 (- x 3)) 48) (+(* 9 y) 48)) 8 8 "ForestGreen")
      ];Fin begin
;De lo contrario
    (void)
);Fin if (>= counterSospechoso 3)
  
(if (< y 99 )
    (if (< x 99 )
        (if (PuntoSospechoso vec x y z)
              (LineasSospechosas vec  (+ x 1) y z (+ counterSospechoso 1) )
        ;De lo contrario
              (LineasSospechosas vec  (+ x 1) y z 0)
        );Fin if (PuntoSospechoso str x y z)
     ;De lo contrario
        (LineasSospechosas vec  1 (+ y 1) z 0)
     );Fin if (< x 99 )
  ;De lo contrario
     (void)
  );Fin if (< Y 99 )
);Fin funcion LineasSospechosas
;Llamamos la funcion
(LineasSospechosas vect x y z counterSospechoso)
);Fin funcion AbrirGrafico2


;Funcion Todas que dibuja las 98 ventanas de la resonancia
(define (Todas counter)
(if (< counter 99)
 [begin
   ;Llamamos la función AbrirGrafico2
   (AbrirGrafico2 vectorCerebro 1 1 counter 0)
   (Todas (+ counter 1))
 ];Fin begin
 (void)
);Fin if (< counter 99)
);Fin funcion Todas
;Llamamos la funcion Todas  
(Todas 1)
);Fin funcion TotalLineasSospechosas

;FuncionInformeResonancia
(define (InformeResonancia)
#|--------------------------------------------------------------------------
Funcion Informe que Imrpime el informe final de toda la resonancia.
- Identificador local vect: Guarda el vector de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterPuntosSospechosos: que cuenta la cantidad de puntos sospechosos
- Identificador local counterPuntosSeguidos: que cuenta la cantidad de puntos sospechosos seguidos
- Identificador local counterLineas: que cuenta la cantidad de lineas sospechosas
|#  
(define (Informe vect x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
(newline)
(printf "---------------------------------------------------------------------------------------------------\n")
(printf "                                        INFORME FINAL                                              \n")
(printf "---------------------------------------------------------------------------------------------------\n")
(printf " Plano en Z(Fotografía)  | Líneas sospechosas x plano(foto) | Puntos sospechosos por plano(foto) \n")
(printf "---------------------------------------------------------------------------------------------------\n")
#|--------------------------------------------------------------------------
Funcion local InformeUnaPagina que Imrpime el informe final de una pagina de la resonancia.
- Identificador local vec: Guarda el vector de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterPuntosSospechosos: que cuenta la cantidad de puntos sospechosos
- Identificador local counterPuntosSeguidos: que cuenta la cantidad de puntos sospechosos seguidos
- Identificador local counterLineas: que cuenta la cantidad de lineas sospechosas
|#  
(define (InformeUnaPagina vec x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
(if (= counterPuntosSeguidos 3)
    (if (< y 99 )
        (if (< x 99 )
            (if (PuntoSospechoso vec x y z)
                (InformeUnaPagina vec (+ 1 x) y z (+ 1 counterPuntosSospechosos) (+ 1 counterPuntosSeguidos) (+ counterLineas 1))
            ;De lo contrario
                (InformeUnaPagina vec (+ 1 x) y z counterPuntosSospechosos 0 (+ counterLineas 1))
            );Fin if (PuntoSospechoso str x y z)
       ;De lo contrario
            (InformeUnaPagina vec 1 (+ y 1) z counterPuntosSospechosos 0 counterLineas)
       );Fin if (< x 99 )
   ;De lo contrario
       [begin
         ( if (= (quotient (+ z 1) 10) 0)
              [begin     
                (printf (string-append "             ~a           |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a                \n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         ;De lo contrario
              [begin     
                (printf (string-append "             ~a          |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a                \n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         );Fin if (= (quotient (+ z 1) 10) 0)
       ];Fin begin
   );Fin if (< y 99 )
   (if (< y 99 )
       (if (< x 99 )
           (if (PuntoSospechoso vec x y z)
               (InformeUnaPagina vec (+ 1 x) y z (+ 1 counterPuntosSospechosos) (+ 1 counterPuntosSeguidos) counterLineas)
           ;De lo contrario
               (InformeUnaPagina vec (+ 1 x) y z counterPuntosSospechosos 0 counterLineas)
           );Fin if (PuntoSospechoso str x y z)
       ;De lo contrario
           (InformeUnaPagina vec 1 (+ y 1) z counterPuntosSospechosos 0 counterLineas)
       );Fin if (< x 99 )
       [begin
         ( if (= (quotient (+ z 1) 10) 0)
              [begin     
                (printf (string-append "             ~a           |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a\n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         ;De lo contrario
              [begin     
                (printf (string-append "             ~a          |                  ~a"(make-string ( - 16 (string-length (~a counterLineas))) #\space)"|                  ~a\n") (+ z 1) counterLineas counterPuntosSospechosos)
                (printf "---------------------------------------------------------------------------------------------------\n")
              ];Fin begin
         );Fin if (= (quotient (+ z 1) 10) 0)
       ];Fin begin
  );Fin if (< y 99 )
);Fin if (= counterPuntosSeguidos 3)
)

 #|--------------------------------------------------------------------------
Funcion local InformeCompleto que Imrpime el informe final de todas las paginas de la resonancia de manera recursiva.
- Identificador local vector: Guarda el vector de la funcion
- Identificador local x: que guarda el valor de x en el plano
- Identificador local y: que guarda el valor de y en el plano
- Identificador local z: que guarda el valor de z en el plano
- Identificador local counterPuntosSospechosos: que cuenta la cantidad de puntos sospechosos
- Identificador local counterPuntosSeguidos: que cuenta la cantidad de puntos sospechosos seguidos
- Identificador local counterLineas: que cuenta la cantidad de lineas sospechosas
|#   
(define (InformeCompleto vector x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
(if (<= z 98)
    [begin
      (InformeUnaPagina vector x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
      (InformeCompleto vector x y (+ z 1) counterPuntosSospechosos counterPuntosSeguidos counterLineas)
    ];Fin befin
;De lo contrario
    (void)
);Fin if (<= z 98)
);Fin funcion InformeCompleto
;Llamamos la funcion InformeCompleto
(InformeCompleto vect x y z counterPuntosSospechosos counterPuntosSeguidos counterLineas)
);Fin funcion Informe
;Llamamos la funcion Informe
(Informe vectorCerebro 1 1 1 0 0 0)
);Fin funcion InformeResonancia

;Llamamos al menú de la funcion
(define (menu)
(printf "\nBienvenido! Porfavor digita el número de la opción a usar:\n")
(printf "1- Analizar punto (x, y, z)\n")
(printf "2- Analizar plano (z)\n")
(printf "3- Mostrar informe\n")
(printf "4- Volver atrás\n")

;Funcion respuesta1 que permite digitar y evaluar la respuesta
(define (respuesta1)
;Identificador option que recibe el valor digitado or el usuario por teclado
(define option (read))
    (if (= option 1)
        [begin
        (TotalPuntosSospechosos)
        (menu)
        ]
    ;De lo contrario
        (if (= option 2)
            [begin
            (TotalLineasSospechosas)
            (menu)
            ]
        ;De lo contrario
            (if (= option 3)
                [begin
                (InformeResonancia)
                (menu)
                ]
            ;De lo contrario
                (if (= option 4)
                    (Menu)
                ;De lo contrario
                    [begin
                      (printf "\nNo es valida la respuesta, escriba otro número: ")
                      (respuesta1)
                    ]
                );Fin if (= option 4)   
            );Fin if (= option 3)
        );Fin if (= option 2)
    );Fin if (= option 1)
);Fin funcion respuesta1
;Llamamos la funcion respuesta1
(respuesta1)
  )
 (menu)
);Fin función Vector

;Funcion menu que imprimirá las opciones para escoger el modo a trabajar y escogerlo
(define (Menu)
(printf"\nEscriba el número de el modo a operar:\n")
(printf "1- Modo vector\n")
(printf "2- Modo strings\n")
;Funcion respuesta que permite digitar un valor por el usuario y evaluar si esta entre las opciones
(define (respuesta)
;Identificador modo que guardará las respuesta del usuario
(define modo (read))
    (if (= modo 1)
        (Vector)
    ;De lo contrario
        (if (= modo 2)
            (String)
        ;De lo contrario
            [begin
            (printf "\nNo es valida la respuesta, escriba otro número: ")
            (respuesta)
            ];Fin begin
        );Fin if (= modo 2)
    );Fin if (= modo 1)
);Fin funcion respuesta
;LLamamos a la funcion respuesta
(respuesta)
);Fin funcion Menu

;Llamamos la funcion del menú
(Menu)