#lang racket

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

; Implementing the expression (2*5)+((8/7)/(5+13)) in Racket.
(define ex1 (+ (* 2 5) (/ (/ 8 7) (+ 5 13))))

; Implementing the sequence of evaluations of ex1 down to a value.
; 1st: computing (2*5)
; 2nd: computing (8/7)
; 3rd: computing (5+13)
; 4th: computing (8/7)/18
; 5th: computing 10+(4/63) leaving us with 634/63
(define ex2
  (list
   (+ (* 2 5) (/ (/ 8 7) (+ 5 13)))
   (+ 10 (/ (/ 8 7) (+ 5 13)))
   (+ 10 (/ 8/7 (+ 5 13)))
   (+ 10 (/ 8/7 18))
   (+ 10 4/63)
   634/63))

; Implementing the expression (y*15)*(x-7)>=y+(x-12) in Racket.
(define (ex3 x y)
  (>= (* (* y 15) (- x 7)) (+ y (- x 12))))

;; Constructs a tree from two trees and a value
; Creating function tree which takes left, value, and right
; and returns those parameters as list
(define (tree left value right)
  (list left value right))

;; Constructs a tree with a single node
; Creating function tree-leaf which takes one parameter value
; and returns list with empty, value, and null
(define (tree-leaf value)
  (list empty value null))

;; Accessors
; Creating functions tree-left, tree-value, tree-right
; using functions first and rest, where first returns
; the head of the list and rest returns the tail
(define (tree-left self)
  (first self))

(define (tree-value self)
  (first (rest self)))

(define (tree-right self)
  (first (rest (rest self))))

;; Copies the source and updates one of the fields
; Creating function tree-set-value which takes arguments self and value
; and returns new list with left subtree of self, value, and right subtree of self
(define (tree-set-value self value)
  (list (tree-left self) value (tree-right self)))
; Creating function tree-set-left which takes arguments self and left
; and returns new list with left, value subtree of self, and right subtree of self
(define (tree-set-left self left)
  (list left (tree-value self) (tree-right self)))
; Creating function tree-set-right which takes arguments self and right
; and returns new list with left subtree of self, value subtree of self, and right
(define (tree-set-right self right)
  (list (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
; If self is null, it returns new binary tree leaf with value
; If value of self = value, it updates value of self
; If value is < value of self, it inserts value into left subtree of self and updates self.
; Else if value is >= value of self, it inserts value into right subtree of self and updates it
(define (bst-insert self value)
  (cond [(null? self) (tree-leaf value)]
        [(equal? (tree-value self) value) (tree-set-value self value)]
        [(< value (tree-value self))
         (let ([left-subtree (bst-insert (tree-left self) value)])
           (tree-set-left self left-subtree))]
        [else
         (let ([right-subtree (bst-insert (tree-right self) value)])
           (tree-set-right self right-subtree))]))
;; lambda
; Since we cannot use cond and boolean, I used the and expression to return T/F
; Checking if node is a list, if not, then entire expression is F
; Checking if length of node is >= 3, if not, then entire expression is F
; Checking if (first node) = 'lambda, if not, then entire expression is F
; Checking if (first (rest node)) is not a symbol, if it is, then entire expression is F
; Checking if (first (rest node)) is a list, if not, then entire expression is F
; Checking if all (first (rest node)) elements are symbols using the andmap function, if not, then entire expression is F
; If all expressions above are T, then the entrie expression is T
(define (lambda? node)
    (and (list? node)
         (>= (length node) 3)
         (equal? 'lambda (first node))
         (not (symbol? (first (rest node))))
         (list? (first (rest node)))
         (andmap symbol? (first (rest node)))))
(define (lambda-params node)
  (first (rest node)))
(define (lambda-body node)
  (rest (rest node)))

;; apply
; Since we cannot use cond and boolean, I used the and expression to return T/F
; Checking if l is a list, if not, then entire expression is F
; Checking if length of list is >= 1, if not, then entire expression is F
; Checking if the first element of list is not equal to 'define, if F, then entire expression is F
; Checking if the first element of list is not equal to 'lambda, if F, then entire expression is F
; If all expressions above are T, then the entrie expression is T
(define (apply? l)
  (and (list? l)
       (>= (length l) 1)
       (not (equal? (first l) 'define))
       (not (equal? (first l) 'lambda))))
(define (apply-func node)
  (first node))
(define (apply-args node)
  (rest node))

;; define
; Since we cannot use cond and boolean, I used the or expression to check
; The or expression check whether functions define-basic? or define-func? returns T
; The function define? will also return T if either one returns T.
(define (define? node)
  (or
   (define-basic? node) (define-func? node)))

; Since we cannot use cond and boolean, I used the and expression to return T/F
; Similar with (define (lamda? node) ...) we check if expressions are F, if not, the entire expression is T
(define (define-basic? node)
  (and (list? node)
       (not (equal? (first node) 'null))
       (> (length node) 2)
       (equal? (first node) 'define)
       (symbol? (first (rest node)))))

; Since we cannot use cond and boolean, I used the and expression to return T/F
; Similar with (define (lamda? node) ...) we check if expressions are F, if not, the entire expression is T
(define (define-func? node)
  (and (list? node)
       (>= (length node) 3)
       (equal? 'define (first node))
       (not (empty? node))
       (list? (first (rest node)))
       (not (empty? (first (rest node))))
       (andmap symbol? (first (rest node)))))
