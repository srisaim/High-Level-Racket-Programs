#lang errortrace typed/racket

(require "hw4-util.rkt")
(provide (all-defined-out))

; Created stream-first function for stream-fold
(: stream-first
  (All [Elem] ; Parameterized on the type of the elements of the stream
    (->
     ; The input is a stream of elements
     (stream Elem)
     ; The output is a element
     Elem)
  )
)
(define (stream-first s)
  (match (s)
    [(stream-add f _) f])
)

; Created stream-rest function for stream-skip
(: stream-rest
  (All [Elem] ; Parameterized on the type of the elements of the stream
    (->
      ; The input is a stream of elements
      (stream Elem)
      ; The output is a stream of elements
      (stream Elem)
    )
  )
)
(define (stream-rest s)
  (match (s)
    [(stream-add _ r) r])
)

(: stream-skip
  (All [Elem] ; Parameterized on the type of the elements of the stream
    (->
      ; The first parameter is the number of elements we wish to skip
      Real
      ; The input is a stream of elements
      (stream Elem)
      ; The output is a stream of elements
      (stream Elem)
    )
  )
)
; Stream-skip takes n (num of elems) & s (stream)
(define (stream-skip n s)
  ; Using let to define recursive function loop
  ; Loop takes current n & s, and is recursive until n = 0
  ; Skips the first elem of stream after each recursive call
  (let loop ((n n) (s s))
    ; If the current n = 0, then return stream s
    ; Else recursively call loop with n decremented by 1 & rest of s
    (if (= n 0)
        s
        (loop (- n 1) (stream-rest s))))
)

(: stream-fold
  ; We have 2 type parameters,
  ; 1. the type of elements of the stream
  ; 2. the type of the result being accumulated
  (All [Elem Accum]
    (->
      ; The type of the step function f
      (-> Elem Accum Accum)
      ; The type of the value being accumulated
      Accum
      ; The input stream of elements
      (stream Elem)
      ; The output stream of folded elements
      (stream Accum)
    )
  )
)
(define (stream-fold f a s)
  (error "todo")
)

; Simply return the empty set
(: set-void set)
(define (set-void)
  (set-empty)
)

; Simply return a set containg the empty string ""
(: set-epsilon set)
(define (set-epsilon)
  (set-add "" set-empty)
)

; Using thunk for a delayed computation the input character x is converted
; to a string and is added to an empty set using the set-add function
(: set-char (-> Char set))
(define (set-char x)
  (thunk (set-add (string x) set-empty)))

; Using thunk for a delayed computation, we pattern match with (s)
(: set-prefix (-> String set set))
(define (set-prefix u s)
  (thunk
    (match (s)
      ; If (s) is empty, then return an empty set
      ; If (s) is not empty, the first elem of (s) is first and the rest of (s) is rest
      ((set-empty) (set-empty))
      ((set-add first rest)
       ; Set-add adds to new set
       ; Using string-append, concat u with first, which is first elem of (s)
       ; Then recursively call set-prefix using u and rest, which are the rest of (s)
       (set-add (string-append u first)
                (set-prefix u rest)))))
)

; Using thunk for a delayed computation, we pattern match with (p1)
(: set-union (-> set set set ))
(define (set-union p1 p2)
  (thunk
    (match (p1)
      ; If (p1) is empty, then return p2
      ; If (p1) is not empty, the first elem of (p1) is first and the rest of (p1) is rest
      ((set-empty) (p2))
      ((set-add first rest)
       ; Set-add adds first elem of p1 to new set
       ; Then recursively call set-union using p2 and rest, which is rest of (p1)
       (set-add first (set-union p2 rest)))))
)

; Using thunk for a delayed computation, we pattern match with (p1)
(: set-concat (-> set set set))
(define (set-concat p1 p2)
  (thunk
   (match (p1)
     ; If (p1) is empty, then return an empty set
     ; If (p1) is not empty, the first elem of (p1) is first and the rest of (p1) is rest
     ((set-empty) (set-empty))
     ((set-add first rest)
      ; Using set-union, find union of both sets
      ; Using set-prefix, the first set will have first, which is first elem of (p1), prefixed with p2
      ; Using set-concat, the second set will recursively concat rest of p1 and p2
      ((set-union (set-prefix first p2) (set-concat rest p2))))))
)  

(: r:eval-exp (-> r:expression Number))
(define (r:eval-exp exp)
  (match exp
    ; If it's a number, return that number
    [(r:number v) v]
    ; Changing (list arg1 arg2), that only takes 2 arguments, to args
    ; If it's a function with any number of arguments, using args
    [(r:apply (r:variable f) args)
      (define func (r:eval-builtin f))
      ; Using map function, it evaluates each argument
      ; Using apply function, it apply evaluated arguments to the function
      (apply func (map r:eval-exp args))
    ]
  )
)

(: r:exp-to-string (-> r:expression String))
(define (r:exp-to-string exp)
  ; Pattern match with exp
  (match exp
    ; Returns string representation of number
    [(r:number v) (number->string v)]
    ; Returns string representation of symbol
    [(r:variable v) (symbol->string v)]
    ; Returns string representation of expression after concating
    [(r:apply f l)
     ; Using string-append to concat, concat ( and string representation of f
     (string-append "(" (r:exp-to-string f)
                    ; If l is empty, then empty string is appended to output
                    (if (empty? l) ""
                        ; Else, using string-append to concat, concat " "
                        ; Map will map arguments to their string representations
                        ; And join function will had a " " after mapping then appended to output
                        ; And string-append ends concat with )
                        (string-append " " (join " " (map r:exp-to-string l)))) ")")]))
