#lang eopl
;Sebastián Orrego Marín - 1941144
;Franklin Aguirre - 1841743
;Repositorio Github: https://github.com/Som1326/Taller_3_FLP.git

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>      ::= <expresion>
;;                      un-programa (exp)
;;  <expresion>     ::= <numero>
;;                      numero-lit (num)
;;                  ::= "\""<texto> "\""
;;                      texto-lit (txt)
;;                  ::= <identificador>
;;                      var-exp (id)
;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      primapp-bin-exp (exp1 prim-binaria exp2)
;;                  ::= <primitiva-unaria> (<expresion>)
;;                      primapp-un-exp (prim-unaria exp)
;;  <primitiva-binaria>     ::= + (primitiva-suma)
;;                  ::= ~ (primitiva-resta)
;;                  ::= / (primitiva-div)
;;                  ::= * (primitiva-multi)
;;                  ::= concat (primitiva-concat)
;;  <primitiva-unaria>      ::= longitud (primitiva-longitud)
;;                  ::= add1 (primitiva-add1)
;;                  ::= sub1 (primitiva-sub1)

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comentario
   ("%" (arbno (not #\newline))) skip)
  (identificador
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
  (texto
   ("\"" letter (arbno (or letter digit "?")) "\"") symbol)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion (texto) texto-lit)
    (expresion (identificador) var-exp)
    (expresion
     ("(" expresion primitiva-binaria expresion ")")
     primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")")
     primapp-un-exp)
    (expresion
     ("Si" expresion "entonces" expresion "sino" expresion "finSi")
     condicional-exp)
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop ">> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (expre env)
    (cases expresion expre
      (numero-lit (numero) numero)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))
      (primapp-bin-exp (exp1 prim-binaria exp2) (apply-bin-primitive (eval-expression exp1 env) prim-binaria (eval-expression exp2 env)))
      (primapp-un-exp (prim-unaria exp) (apply-un-primitive prim-unaria (eval-expression exp env)))
      (condicional-exp (test-exp true-exp false-exp)
              (if (valor-verdad? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      )))

;apply-bin-primitive: <expression> <primitiva-binaria> <expression> -> numero
(define apply-bin-primitive
  (lambda (exp1 prim-binaria exp2)
    (cases primitiva-binaria prim-binaria
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-multi () (* exp1 exp2))
      (primitiva-concat () (cons exp1 exp2)))))

;apply-un-primitive: <primitiva-unaria> <expression> -> numero
(define apply-un-primitive
  (lambda (prim-unaria exp)
    (cases primitiva-unaria prim-unaria
      (primitiva-longitud () (length exp))
      (primitiva-add1 () (+ exp 1))
      (primitiva-sub1 () (- exp 1)))))

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "Error, la variable no existe" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym)))))))

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;Función length que retorna la longitud de una lista

(define (length lst)
  (cond
    [(null? lst)  0]
    [else   (+ 1 (length (cdr lst)))]))