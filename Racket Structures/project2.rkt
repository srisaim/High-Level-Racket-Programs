#lang racket

(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1
;; Implementing pair using struct
(struct pair (left right))

;; Creating pair-lhs and rhs that holds left and right side elements
(define pair-lhs pair)
(define pair-rhs pair)


;; Exercise 1.a
(define (pair-set-left p l) 'todo)

;; Exercise 1.b
(define (pair-set-right p r) 'todo)

;; Exercise 1.c
(define (pair-swap p) 'todo)

;; Exercise 1.d
;; You can only use match* one time. You cannot use match.
(define (pair-add p1 p2) 'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.a
;; Using lambda function that takes argument "op"
;; Using conditional statment to check value of op
;; If "op" is equal to first, it returns first argument
;; If "op" is equal to last, it returns last argument
;; If "op" is equal to full, it returns first and last with space in middle using string-append
;; If "op" is equal to initials, it returns first letter of first and last concated using substring
;; If "op" is none of the above, an error occurs
(define (name first last)
  (lambda (op)
    (cond ((equal? op 'first) first)
          ((equal? op 'last) last)
          ((equal? op 'full) (string-append first " " last))
          ((equal? op 'initials) (string-append (substring first 0 1) (substring last 0 1)))
          (else (error "error")))))

;; Exercise 2.b
;; Simply returns p with argument first
(define (first-name p)
  (p 'first))

;; Exercise 2.c
;; Simply returns p with argument last
(define (last-name p)
  (p 'last))

;; Exercise 2.d
;; Simply returns p with argument full
(define (full-name p)
  (p 'full))

;; Exercise 2.e
;; Simply returns p with argument initials
(define (initials p)
  (p 'initials))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3
;; Using match, we pattern-match with list "l"
;; If the list is empty, return "n"
;; If the list is not empty, using cons, first element is "a" and the rest of the elements are "b"
;; Then recursively call max-from with "n" and "a", and "b" being rest of the list
;; The process repeats as we take the max of the current and next number in list,
;; until function returns max number in list
(define (max-from n l)
  (match l
    ((list) n)
    ((cons a b) (max-from (max n a) b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4
;; Using match, we pattern-match with list "l"
;; If the list is empty, return "n"
;; If the list is not empty, using cons, first element is "a" and the rest of the elements are "b"
;; If "a" is greater than "n", then min value is in "b", so recursively call min-from with "n" and "b"
;; If "n" is greater than or equal to "a", then min value is "a", so recursively call min-from with "a" and "b"
;; where "a" is new starting num and "b" is rest of the list
(define (min-from n l)
  (match l
    ((list) n)
    ((cons a b) (if (< n a)
                    (min-from n b)
                    (min-from a b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5: revisit Exercise 3 and Exercise 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6
;; Using match, we pattern-match with parameter "l"
;; If the list is empty, return 0
;; If the list is not empty, using cons, match list with a single element, where "_" acts
;; as a blank value and ignores, and "b" is assigned the rest of the list
;; Then recursively call count on "b", which is rest of the list, and add 1 to result
(define (count l)
  (match l
    ((list) 0)
    ((cons _ b) (+ 1 (count b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7
;; Using match, we pattern-match with parameter "l"
;; If the list is empty, return 0
;; If the list is not empty, using cons, first element is "a" and the rest of the elements are "b"
;; Then recursively call sum on "b", which sums up the rest of the list and add "a" to the result
(define (sum l)
  (match l
    ((list) 0)
    ((cons a b) (+ a (sum b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 8
;; Using match, we pattern-match with parameter "l"
;; If the list is empty, return 0
;; If the list is not empty, using cons, first element is "a" and the rest of the elements are "b"
;; If "x" is equal to "a", the value of occurrence is 1 and add that to the result of recursively
;; calling occurrence to the rest of the list, which is "b"
;; If "x" is not equal to "a", then the value of occurrence is the result of recursively calling occurrence on "b"
(define (occurrences x l)
  (match l
    ((list) 0)
    ((cons a b) (if (equal? x a)
                    (+ 1 (occurrences x b))
                    (occurrences x b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 9
;; Defining a helper function "h" which takes a list of numbers "nums"
;; Using match, we pattern-match with the list "nums"
;; If the list is empty, return 0
;; If the list is not empty, using cons, first element is "a" and the rest of the elements are "b"
;; Then recursively call helper function "h" on "b", which is rest of the list, to compute sum of the squared numbers
;; and add the result to "a" squared, which is the first element in the list
;; Finally compute the square root of the sum of all squared numbers in the list
(define (norm l)
  (define (h nums)
    (match nums
      ((list) 0)
      ((cons a b) (+ (* a a) (h b)))))
  (sqrt (h l)))
