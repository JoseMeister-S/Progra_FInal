#lang play
(print-only-errors #t) ; Para ver solo los errores.

#|
<FAE-L> ::=   <num> | <bool> | <id>
            | (+ <FAE> <FAE>)
            | (- <FAE> <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
|#


(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [add l r]                               ; (+ <FAE> <FAE>)
  [sub l r]                               ; (- <FAE> <FAE>)
  [if-tf c et ef]                         ; (if-tf <FAE> <FAE> <FAE>)
; [with id-name named-expr body-expr]     ; (with <id> <FAE> <FAE>) "syntax sugar"
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <FAE> <FAE>) ; ahora podemos aplicar una funcion a otra
  [fun arg body]   
) 


#|
<env> ::= (mtEnv)
          | (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

; empty-env -> (mtEnv)
(define empty-env (mtEnv))

; extend-env:: <id> <val> <env> -> <env>
(define extend-env aEnv)
; env-lookup :: <id> <env> -> <val>
; buscar el valor de una variable dentro del ambiete
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "undefined: " x)]
    [(aEnv id val tail)(if (eq? id x) val (env-lookup x tail))]
    )
  )


; transform-fundef
(define (transform-fundef arg-names body)
  (if (= 1 (length arg-names))
      (fun (first arg-names) body)
      (fun (first arg-names) (transform-fundef (cdr arg-names) body)))
  )
; (transform-fundef '{a b} (add (id 'a) (id 'b)))


; transform-funapp
(define (transform-funapp fun args)
  (if (= 1 (length args))
      (app fun (first args))
      (app (transform-funapp fun (cdr args)) (car args)))
  )

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (app (fun x (parse b)) (parse e))]
    [(list 'fun arg-names body) (transform-fundef arg-names (parse body))] ; 1. Agregar el caso del fun
    [(list fun args) (match args
                       [(? number?) (app (parse fun) (parse args))]
                       [(? boolean?) (app (parse fun) (parse args))]
                       [(? symbol?) (app (parse fun) (parse args))]
                       [(cons head tail) (if (symbol? (first args))
                                             (app (parse fun) (parse args))         
                                             (transform-funapp (parse fun) (reverse (map parse args))))]
                       )
     ]
    ;[(list arg e) (app (parse arg) (parse e))]; 2. Subir de nivel nuestras funciones
    )
  )
; (parse '{fun {a b} {+ a b}} 2 1)

;(app (app (fun 'a (fun 'b (add (id 'a) (id 'b)))) (num 1)) (num 3))
;(app (app (fun 'a (fun 'b (add (id 'a) (id 'b)))) (num 1)) (num 3))


(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env
  (promiseV expr env cache) ; promise = expr-L + env + cache
  )

; interp :: Expr  Env -> Val
; interpreta una expresion
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(add l r) (valV+ (strict (interp l env)) (strict (interp r env)))]
    [(sub l r) (valV- (strict (interp l env)) (strict (interp r env)))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    ;[(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(app f e)
     (def (closureV arg body fenv) (strict (interp f env))) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body
    
     (interp body (extend-env arg
                              (promiseV e env (box #f)) ; lazy eval
                              ;(interp e env) ; eager eval
                              fenv)) ; parece que no funciona ni con estatico ni dinamico
     ]
))

; (run '{{fun {a b} {+ a b}} 1 3})

; valV+ : Val -> Val
(define (valV+ s1 s2)
  (valV (+ (valV-v s1) (valV-v s2)))
  )

(define (valV- s1 s2)
  (valV (- (valV-v s1) (valV-v s2)))
  )

; strict -> Val(valV/closureV/promiseV) -> Val (valV/closureV))
; destructor de promesas - cumplidor de promesas
(define (strict val)
  (match val
    [(promiseV e env cache)
     (if (unbox cache)
         (begin
           ;(printf "Using cached value~n")
           (unbox cache)
           )
         (let ([interp-val (strict (interp e env))])
           (begin (set-box! cache interp-val)
                  ;(printf "Forcing: ~a~n " interp-val)
                  interp-val))
         )]
    [else val]
    )
  )

; run: Src -> Src
; corre un programa
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    ; (interp res ...)
    (match (strict res)
      [(valV v) v]
      [(closureV arg body env) res])
      ;[(promiseV e env) (interp e env)])
    )
  )

