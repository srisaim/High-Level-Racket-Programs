;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang typed/racket
(require "hw6-util.rkt")
(provide (all-defined-out))
;; END OF REQUIRES

;; Exercise 1
(: eval-exp (-> memory handle d:expression (eff memory d:value)))
(define (eval-exp mem env exp)
  ; mem is M
  ; env is E
  (match exp
    [(? d:value?)
      ; If the expression is value v
      ; Return: v ▶ M (value and unchanged memory)
      (eff mem exp)
      ]
    [(? d:variable?)
      ; If the expression is a variable x
      ; Search for the value of variable in environment
      ; and return it with unchanged memory
      ; Return: E(x) ▶ M
      (eff mem (environ-get mem env exp))
    ]
    [(d:lambda x t)
      ; If the expression is a lambda λx.t
      ; Return a closure with environment, parameter, and body with unchanged memory
      ; Return: {E, λx.t} ▶ M
      (eff mem (d:closure env x t))
    ]
    [(d:apply ef ea)
      ;; Evaluating the func expression in the environment (ef and E)
      (match (eval-exp mem env ef)
        ;; If the eval function is closure Ef, λx.tb
        ;; and memory is M1
        ;; ef ⇓E {Ef, λx.tb} ▶ M1
        [(eff M1 (d:closure Ef x tb))
          ;; Evaluate arg expression ea
          ;; ea ⇓E va ▶ M2
          (match (eval-exp M1 env ea)
            ;; If the eval arg is a value va
            ;; and the memory is M2
            [(eff M2 va)
            ;; Create new environement Eb using push
            ;; Binding x := a to Ef, and updating memory to M3
            ;; Eb ← Ef + [x := a] ▶ M3
            (match (environ-push M2 Ef x va)
              [(eff M3 Eb)
               ;; Eval body of lambda in new environment (tb and Eb)
               ;; tb ⇓Eb vb ▶ M4
               (match tb
                 [(? d:expression?)
                  (eval-exp M3 Eb tb)])
               ;; Return: vb ▶ M4
               ])
            ])
         ]
       )
     ]
   )
)

;; Exercise 2

(: eval-term (-> memory handle d:term (eff memory d:value)))
(define (eval-term mem env term)
  (match term
    [(d:define x e)
      ;; Eval expression in the environment (e and env)
      (match (eval-exp mem env e)
         ;; If the eval equals value and memory (v and M1)
         ;; e ⇓E v ▶ M1
         [(eff M1 v)
         ;; Update environment by binding var x and value v
         ;; Environment updates to M2
         ;; E ← [x := v] ▶ M2
         (eff (environ-put M1 env x v) (d:void))
         ;; return: void ▶ M2
         ])
    ]
    [(d:seq t1 t2)
     ;; Eval first term in env and memory
     (match (eval-term mem env t1)
       ;; ​t1​ ⇓E ​v1 ▶ M1
       ;; After eval t1, memory state is updated to M1
       ;; Get value of t1 in v1, which isn't going to be used
       [(eff M1 v1)
        ;; t2 ⇓E v2 ▶ M2
        ;; Eval t2, second term, in updated env and M1
        ;; Value of t2 and M2 will be returned
        (eval-term M1 env t2)
        ;; return: v2 ▶ M2
        ])
    ]
    [(? d:expression?) (eval-exp mem env term)]
  )
)

;; Exercise 3 (Manually graded)
#|
Considering how definitions work in Racket and our specification of Language λD, variables can be bound to values, as both Racket and Language λD follow lexical scoping.
But the main difference between Racket and Language λD is that Racket's variable binding is maintained at the time when the function is defined.
The variables definition will stay the same throughout the program until it is redefined. The behavior of the functions won't change if the variable is later redefined,
allowing for more complex programs. Since closures in Language λD do not capture the environment, variable bindings can change when a function is called.
If the variable was changed between the time the function was defined and called, it may produce incorrect outputs when used in different functions.
Therefore, variables to be used in functions have to be defined multiple times to get the correct output.

An example for Racket:
(define a 3)                  ; a is defined 3
(define b (lambda () a))      ; b is defined without args and returns value of a
(define a 6)                  ; a is redefined 6
(b)                           ; b is called and returns 6 in Racket because it was redefined to 6

An example for Language λD:
;; Returns 3 in Language λD
(define a 3)                  ; a is defined 3
(define b (lambda () a))      ; b is defined without args and returns value of a
(define a 6)                  ; a is redefined 6
(b)                           ; b is called and returns 3 in Racket because it finds when x was defined first in environment

Example shows how Racket and Language λD handle variable bindings differently.
|#
