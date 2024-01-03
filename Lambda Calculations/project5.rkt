;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace typed/racket
(provide (all-defined-out))
(require "hw5-util.rkt")
;; END OF REQUIRES

;; Exercise 1
(: s:subst (-> s:expression s:variable s:value s:expression))
(define (s:subst exp var val)
  ; Using a cond expression
  (cond
    ; If exp is a number, return exp
    ; If exp is a variable, check if same as var
    [(s:number? exp) exp]
    [(s:variable? exp)

     ; If the name of exp is equal to name of var, replace exp with val
     ; Otherwise, return exp
     (if (equal? (s:variable-name exp) (s:variable-name var)) val exp)]

    ; If exp is lambda expression, check if the param is the same as var
    [(s:lambda? exp)

     ; If the name of lambda-param is equal to name of var, return exp
     ; Otherwise, create a new lambda expression with same param and substituted body
     ; The body is by calling s:subst recursively on orginal lambda body
     (if (equal? (s:variable-name (s:lambda-param exp)) (s:variable-name var))
         exp (s:lambda (s:lambda-param exp) (s:subst (s:lambda-body exp) var val)))]

    ; If exp is a func application, substitute var in both the func and arg
    [(s:apply? exp)
     ; Create a new func application with substituted func and arg
     ; By calling s:subst recursively on orginal func application with var and val
     (s:apply (s:subst (s:apply-func exp) var val) (s:subst (s:apply-arg exp) var val))]
    )
)

;; Exercise 2
(: s:eval (-> (-> s:expression s:variable s:value s:expression) s:expression s:value))
(define (s:eval subst exp)
  (match exp
    [(s:apply ef ea)
      (match (s:eval subst ef)
        [(s:lambda x eb)
          ; ... define 1
         ; define va with the result of evaluating ea with given subst
         (define va (s:eval subst ea))
          ; ... define 2
         ; define vb with the result of evaluating eb after replacing va for x using subst
         (define vb (s:eval subst (subst eb x va)))
          ; Simply return vb
         vb
        ]
      )
    ]
    ; Pattern checks if exp is a value
    [(? s:value?) exp]
    )
)

;; Exercise 3
(: e:eval (-> e:environ e:expression e:value))
(define (e:eval env exp)
  ; Using a cond expression
  (cond
    ; If exp is a value, return exp
    ; If exp is a variable, get its value in environment
    ; If exp is a func application, evaluate the function and argument
    [(e:value? exp) exp]
    [(e:variable? exp) (e:env-get env exp)]
    [(e:apply? exp)

     ; Evaluate the function and argument in the current environment
     ; Store the resulting values in func and arg variables
     (define func (e:eval env (e:apply-func exp)))
     (define arg (e:eval env (e:apply-arg exp)))

     ; Checking if the evaluated func is a closure
     (if (not (e:closure? func))
         ; If it's not a closure, then an error is thrown
         (error "expression not a function")
         ; Otherwise, create a new environment with the closure param bound to the evaluated arg
         (let ([new-env (e:env-put (e:closure-env func) (e:closure-param func) arg)])
           ; Evaluate the closure body in the new environment
           (e:eval new-env (e:closure-body func))))]

    ; If exp is a lambda expression, return a closure with the current environment
    [(e:lambda? exp)
     (e:closure env (e:lambda-param exp) (e:lambda-body exp))]
    )
)

;; Exercise 4 (Manually graded)
#|
When it comes to λS (call-by-name), lambda-calculus substitutes a parameter into the function body without evaluating it.
On the other hand, for λE (call-by-value), lambda calculus needs the parameter to be evaluated to a value before being substituted.

In a scenario where λS (call-by-name) is better than λE (call-by-value):
     - A great example can be the conditional statements in Racket, (cond).
       The conditional statements hold multiple cases and arguments, but sometimes only a few are met.
       So using λS (call-by-name) is a better evaluation strategy as it can be more efficient as it skips unused arguments,
       which cause extra computation and only computes necessary arguments, making it more efficient.
       We usually see this in lazy computation and boolean expressions.
       Long data structures also use λS, as it only computes necessary parts in the data structure instead of evaluating all.

In a scenario where λE (call-by-value) is better than λS (call-by-name):
     - When a function takes numerous arguments and must use all of them,
       using the λE (call-by-value) evaluation strategy is preferable because it is more efficient.
       This is due to the strategy's ability to analyze each argument only once before passing it to the function.
       This approach prevents extraneous computations and evaluations.
       When you're repeating an argument or having complex computations, λE is better as it efficiently computes only once when called.
|#

;; Exercise 5 (Manually graded)
#|
There are many advantages to using formal specifications, particularly when implementing software systems.
The first advantage is that formal specifications can help you comprehend the code and make its behavior more understandable.
It improves developers' understanding of how code should function and lowers the possibility of human error,
both of which can improve the development and execution process. Another advantage is that it increases the software's effectiveness.
Formal standards for expressions and syntaxes are made to be accessible and simple to comprehend.
Developers can check and confirm their implementation using a precise set of criteria.
Furthermore, a formal specification can be used to generate test inputs that cover a variety of situations and edge cases,
enhancing the overall quality and robustness of the software system.
|#
