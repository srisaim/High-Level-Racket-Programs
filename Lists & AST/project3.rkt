#lang racket

(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1
; foldr will iterate over l
; lambda returns minimum value of h and result, minimum value in l
; n is base value
(define (min-from n l)
  (foldr
   ; handle-step
   (lambda (h result) (min h result))
   ; base step
   n
   ; iterate over l
   l))

;; Exercise 2
; foldl will iterate over l
; lambda returns sum of 1 and accum, counting all elements in l
; 0 is base value
(define (count l)
  (foldl
   ; handle-step
   (lambda (h accum) (+ 1 accum))
   ; base step
   0
   ; iterate over l
   l))

;; Exercise 3
; foldr will iterate over l
; lambda returns sum of h and accum, adding all elements in l
; 0 is base value
(define (sum l)
  (foldr
   ; handle-step
   (lambda (h accum) (+ h accum))
   ; base step
   0
   ; iterate over l
   l))

;; Exercise 4
; foldr will iterate over l
; lambda checks if current h = to the n we're looking for
; if h = n, then add 1 to accum, counting the occurrence in l
; 0 is base value
(define (occurrences n l)
  (foldl
   ; handle-step
   (lambda (h accum)
     (if (equal? n h)
         (+ 1 accum)
         accum))
   ; base step
   0
   ; iterate over l
   l))

;; Exercise 5
; map will iterate over l, applying lambda function and returns results
; lambda appends s to current h using string-append, which concates strings
; so lambda is used to add prefix s to each element in l
(define (prefix s l)
  (map
   (lambda (h) (string-append s h)) l))

;; Exercise 6
; match is pattern-matching
; if l1 is an empty list, return l2
; else it takes first element of l1, and conses it with the result of
; recursively calling interleve with l2 and the rest of l1
; function alternates between l1 and l2 as it interleaves a new list
(define (interleave l1 l2)
  (match l1
    [(list)
      l2]
    [(list h1 l1 ...)
      (cons h1 (interleave l2 l1))])
)

;; Exercise 7
; foldr will iterate over l
; lambda checks if accum is empty, and if it is, it returns a list containing only h
; else, it will return a list containing v, then list containing h and accum
; 0 is base value
(define (intersperse l v)
  (foldr
   ; handle step
   (lambda (h accum)
          (if (empty? accum)
              (cons h accum)
              (cons v (cons h accum))))
   ; base step
   0
   ; iterate over l
   l))

;; Exercise 8
; functions that create AST
(define/contract (parse-ast node)
  (-> any/c r:term?)

  ; Defined name using parse-ast
  ; Defined param with map parse-ast, that go from list of symbols to list of variables
  ; Defined body with map parse-ast, that go from list of quoted terms to list of terms
  ; Return an r:define with name and r:lambda, that holds param and body
  (define (make-define-func node)
    (define name (parse-ast (first (second node))))
    (define param (map parse-ast (rest (second node))))
    (define body (map parse-ast (rest (rest node))))
    (r:define name (r:lambda param body)))

  ; Defined name using parse-ast
  ; Defined body using parse-ast
  ; Return an r:define with name and body
  (define (make-define-basic node)
    (define name (parse-ast (second node)))
    (define body (parse-ast (third node)))
    (r:define name body))

  ; Defined param with map parse-ast, that go from list of symbols to list of variables
  ; Defined body with map parse-ast, that go from list of quoted terms to list of terms
  ; Return an r:lambda with param and body
  (define (make-lambda node)
    (define param (map parse-ast (second node)))
    (define body (map parse-ast (rest (rest node))))
    (r:lambda param body))

  ; Defined func using parse-ast
  ; Defined args using map parse-ast, that goes from list of quoted expressions to list of expressions
  ; Return an r:apply with func and args
  (define (make-apply node)
    (define func (parse-ast (first node)))
    (define args (map parse-ast (rest node)))
    (r:apply func args))

  ; Defined from lecture, returns correctly syntaxed r:number node
  (define (make-number node)
    (r:number node))

  ; Similar to make-number, returns correctly syntaxed r:variable node
  (define (make-variable node)
    (r:variable node))

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
