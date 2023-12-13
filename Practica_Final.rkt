#lang play
;FAE-JN
(print-only-errors #f) ; Para ver solo los errores.

(define primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '< <)
   (cons '> >)
   (cons '<= <=)
   (cons '>= >=)
   (cons '== =)
   (cons '!= (λ (x y) (not (equal? x y))))
   (cons '&& (λ (x y) (and x y)))
   (cons '|| (λ (x y) (or x y)))
   (cons 'append (λ (str1 str2) (string-append str1 str2)))
   (cons 'eq-str? (λ (str1 str2) (string=? str1 str2)))
   (cons 'contains (λ (x y) (string-contains? x y)))
   ))


(deftype Expr
  [num n]                                
  [bool b]                                
  [if-tf c et ef]                         
  [id name]                                
  [app fname arg-expr]                    
  [fun arg body]
  [prim name args]
  [lazy-app arg body]
  [prim-L body]
  [str s]
) 



(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

; empty-env -> (mtEnv)
(define empty-env (mtEnv))

; extend-env:: <id> <val> <env> -> <env>
(define extend-env aEnv)

; env-lookup :: <id> <env> -> <val>
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



; transform-funapp
(define (transform-funapp fun args)
  (if (= 1 (length args))
      (app fun (first args))
      (app (transform-funapp fun (cdr args)) (car args)))
  )

;typeof: expr -> type/error
(define (typeof expr)
  (match expr
    [(num n) (num)]
    [(bool b) (bool)]
    [(str s) (str)]
    [(list name vals)
     (match name 
       ['+  (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['- (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['* (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['/ (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['> (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['< (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['<= (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['>= (if (andmap number? vals)
                     (void)
                     (error "type error"))]
       ['== void]
       ['!= void]
       ['&& void]
       ['|| void]
       ['append (if (andmap string? vals)
                     (void)
                     (error "type error"))]
       ['eq-str? (if (andmap string? vals)
                     (void)
                     (error "type error"))]
       ['contains (if (andmap string? vals)
                     (void)
                     (error "type error"))]
       )]
    )
  )

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(? string? s) (str s)]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    ;por alguna razon no funciona bien si no es con with N
    [(list 'with args body)
     (match args
       [(list x e) (app (fun x (parse body)) (parse e))]
       [(cons head tail) (app (fun (car head)(if (eq? tail null)
                              (parse body)
                              (parse (list 'with tail body))))
                                      (parse(cadr head)))]
       )
     ]
    [(list 'rec (list x e) b)(parse `{with {,x {Y {fun {,x} ,e}}} ,b})]
    [(list 'withN (cons head tail) body) (app (fun (car head)(if (eq? tail null)
                              (parse body)
                              (parse (list 'withN tail body))))
                                      (parse(cadr head)))]
    [(list 'lazy (list x e) b) (lazy-app (fun x (parse b)) (parse e))]
    [(list 'delay body) (prim-L body)]
    [(list 'force args)
           (match args
           [(list t (cons prim-name args)) (prim prim-name (map parse args))]
           [(cons prim-name args) (prim prim-name (map parse args))])]
    [(list 'fun arg-names body) (transform-fundef arg-names (parse body))]
    [(list fun args) (match args
                       [(? number?) (app (parse fun) (parse args))]
                       [(? boolean?) (app (parse fun) (parse args))]
                       [(? symbol?) (app (parse fun) (parse args))]
                       [(cons head tail) (if (symbol? (first args))
                                             (app (parse fun) (parse args))         
                                             (transform-funapp (parse fun) (reverse (map parse args))))]
                       )
     ]
    [(cons prim-name args) (prim prim-name (map parse args))]
    [(list arg e) (app (parse arg) (parse e))]
    )
  )


;id-lookup :: FAE -> bool
(define (id-lookup expr)
  (match expr
    [(prim prim-name args) (if (empty? (filter symbol? args)) #f #t)]
    )
  )


(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env
  (promiseV expr env) ; promise = expr-L + env
  )

; interp :: Expr  Env -> Val
; interpreta una expresion 
(define (interp expr env)  
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(str s)(valV s)] 
    [(id x) (env-lookup x env)]
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (promiseV x env)) args))]
    [(prim-L body) (promiseV body env)]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(fun arg body) (closureV arg body env)]
    [(app f e)
     (def (closureV arg body fenv) (strict (interp f env))) 
     (interp body (extend-env arg
                              (interp e env) 
                              fenv))
     ]
     [(lazy-app f e)
     (def (closureV arg body fenv) (strict (interp f env)))
     (interp body (extend-env arg
                              (promiseV e env) 
                              fenv)
             )]
))    

; prim-ops: op-name list[Val] -> Val
(define (prim-ops op-name args)
  (let* ([vals (map (λ (x) (valV-v (strict x))) args)]
        [t (typeof (list op-name vals))]
        )
    (valV (apply (cdr (assq op-name primitives)) vals))
    )
  )



; strict -> Val(valV/closureV/promiseV) -> Val (valV/closureV))
; destructor de promesas - cumplidor de promesas
(define (strict val)
  (match val
    [(promiseV e env) (strict (interp e env))]
    [else val]
    )
  )

(define (Y-combinator arg func enviroment)
  (extend-env arg (interp func enviroment) enviroment))

; run: Src -> Src
; corre un programa
(define (run prog)
  (let* ( [res (interp (parse prog) (Y-combinator 'Y (parse '{fun {f} {with {h {fun {g} {fun {n} {{f {g g}} n}}}} {h h}}}) empty-env))])
    ; (interp res ...)
    (if (promiseV? res)
        res
      (match (strict res)
      [(valV v) v]
      [(closureV arg body env) res]
      [(promiseV e env) (interp e env)]
        )
        )
      )
  )
;Tests para 1
(test (run '{append "Hola" "Adios"})"HolaAdios")
(test/exn (run '{append "Hola" 1}) "type error")
(test (run '{eq-str? "Hola" "Adios"}) #f)
(test (run '{eq-str? "Hola" "Hola"}) #t)
(test/exn (run '{eq-str? "Hola" 1}) "type error")
(test (run '{contains "Funcional" "Fu"})#t)
;Tests para 3
(run '{delay {+ 1 1}})
(run '{force {delay {+ 1 1}}})
(run '{force {+ 1 1}})

;Tests para 4
;deberia funcionar, DEBERIA
(run '{rec {sum {fun {n}
                        {if-tf {== 0 n} 0 {+ n {sum {- n 1}}}}}} {sum 0}})
(run '{rec {sum {fun {n}
                        {if-tf {== 0 n} 0 {+ n {sum {- n 1}}}}}} {sum 10}})

 
;Tests para 5
(run '{lazy {a {fun {f} {f 3}}} a})
(run '{with {a {fun {f} {f 3}}} a})
(test (run '{lazy {x y} 1}) 1)
(test (run '{with {x y} 1}) 1)


(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 1 2 3 4}) 10)
(test (run '{* 2 3 4}) 24)
(test (run '{/ 12 2 2}) 3)
(test (run '{< 12 3}) #f)
(test (run '{<= 12 3}) #f)
(test (run '{< 12 12}) #f)
(test (run '{<= 12 12}) #t)
(test (run '{> 12 3}) #t)
(test (run '{>= 12 3}) #t)
(test (run '{> 12 12}) #f)
(test (run '{>= 12 12}) #t)
(test (run '{>= 12 12}) #t)
(test (run '{== 12 12}) #t)
(test (run '{== 12 11}) #f)
(test (run '{!= 12 12}) #f)
(test (run '{!= 12 11}) #t)
(test (run '{&& 12 11}) 11)
(test (run '{&& #f #t}) #f)
(test (run '{|| #f #t}) #t)
(test (run '{|| 12 11}) 12)
(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{* 1 1 1 1}) 1)
(test/exn (run '{* 1 #t 1 1}) "type error")
(test/exn (run '{with {x #t} {* 1 x x x}}) "type error")
(test/exn (run '{with {x #t} {* x x x x}}) "type error")
(test (run '{with {x 3} {+ x x}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{withN {{x 3} {y 2}} {+ x y}}) 5)
(test (run '{withN {{x 3} {x 5}} {+ x x}}) 10)
(test (run '{withN {{x 3} {y {+ x 3}}} {+ x y}}) 9)
(test (run '{withN {{x 10} {y 2} {z 3}} {+ x {+ y z}}}) 15)
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)
(test/exn (run '{f 10}) "undefined")
(test (run '{with {f {fun {x} {+ x x}}}{f 10}}) 20)
(test (run '{{fun {x} {+ x x}} 10}) 20)
(test (run '{with {add1 {fun {x} {+ x 1}}}{add1 {add1 {add1 10}}}}) 13)
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {foo 10}}}) 22)
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {f} {+ {f 10} {f 10}}}}
                        {foo add1}}}) 22)
(test (run '{{fun {x}{+ x 1}} {+ 2 3}}) 6)
(test (run '{with {apply10 {fun {f} {f 10}}}
                  {with {add1 {fun {x} {+ x 1}}}
                        {apply10 add1}}}) 11)
(test (run '{with {addN {fun {n}
                       {fun {x} {+ x n}}}}
            {{addN 10} 20}}) 30)