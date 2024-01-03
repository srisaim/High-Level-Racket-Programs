#lang typed/racket
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
(require "hw7-util.rkt")
(provide (all-defined-out))
;; END OF REQUIRES

;;;;;;;;;;;;;;;
;; Exercise 1
;
; Given a frame, return all handles contained therein.
;
; Use frame-values; only closure's env is relevant to this problem
; Read the hints in the PDF.
(: frame-refs (-> frame (Setof handle)))
(define (frame-refs frm)
  ;; Defining closure,
  (define c
    ;; Filtering the list of values from frm to only consist those that are closures.
    ;; By using d:closure? we can determine if value is a closure.
    (filter d:closure? (frame-values frm)
    )
  )
  ;; Defining envs,
  (define e
    ;; By map, we apply d:closure-env to each element in closure list,
    ;; and return new list that consists of results.
    (map d:closure-env c)
    )
  ;; Defining parent,
  (define p
    ;; Checking if frame-parent isn't false
    (if (frame-parent frm)
        ;; If frm has parent frame, then we create a new list containg parent frame.
        (list (frame-parent frm))
        ;; If frm has no parent frame, returns empty list.
        empty
        )
    )
  ;; Concates envs and parent list using append
  ;; Converts the list to a set using list->set
  (list->set (append e p))
)

;;;;;;;;;;;;;;;
;; Exercise 2
; Standard graph algorithm: return all handles reacheable via `contained`.
; Hint: Consider solving this exercise last, as conceptually the most difficult
;       exercise.
; Hint: This is a simple breadth-first algorith. The algorithm should start
;       in env and obtain the set of next elemens by using `contained`.
; Hint: The algorithm must handle cycles.
; Hint: Do not hardcode your solution to frames (you should test it with
;       frames though)
(: mem-mark
  (All [T]
    (->
      (-> T (Setof handle))
      (heap T)
      handle
      (Setof handle)
    )
  )
)
(define (mem-mark contained mem env)

  ; (: mem-mark-iter (-> (Listof handle) (Setof handle) (Setof handle)))
  ; One solution to this problem is to loop while maintaining
  ; a list of environments to visit, and a set of elements visited
  ; (define (mem-mark-iter to-visit visited)
  ;   ; while not empty(to-visit):
  ;   ;    1. pick one env from from to-visit and retrieve its "frame"
  ;   ;    2. let "c" be the set of env's contained in the given "frame"
  ;   ;    3. let the set "new" be every element that is in "c" but not in "visited" (use set-subtract)
  ;   ;    4. add every element of "new" to the rest of elements to be visited
  ;   ;    5. update the set of visited elements to include the new elements
  ;   (todo "todo")
  ; )
  ; ; run the loop with 1 element to visit, and 1 element visited
  ; (mem-mark-iter (list env) (set env))

  ; The function takes a list of handles to visit (tv), a set of handles visited (v), and returns a set of handles.
  ; tv = to visit | v = visit
  (: mem-mark-iter (-> (Listof handle) (Setof handle) (Setof handle)))
  (define (mem-mark-iter tv v)
    ; Checking if list tv is empty and returns set v if it is.
    (if (null? tv)
        v
        ; If list tv is not empty, first env from list is bound to current
        (let ([current (first tv)])
          ; We get frame associated with current from heap and bind it to frame
          (let ([frame (heap-get mem current)])
            ; We get the set of env contained in current frame and bind it to c
            (let ([c (contained frame)])
              ; Then compute the set of env in c, not visited yet, by subtracting visited set from c
              ; The result is bound to new
              (let ([new (set-subtract c v)])
                ; We then recursively call mem-mark-iter
                ; It updates the tv list by removing the first element and appending new env from new
                ; Using set-union, we update visited set by adding new env from new
                (mem-mark-iter (append (rest tv) (set->list new))
                               (set-union v new)
                )
              )
            )
          )
        )
     )
  )
  ; Setting up initial values and arguments for mem-mark-iter
  (mem-mark-iter (list env) (set env)))

;;;;;;;;;;;;;;;
;; Exercise 3
;
; Return a new heap that only contains the key-values referenced in to-keep.
;
; Tip 1: We have learned a similar pattern than what is asked here.
; Tip 2: The function you want to use starts with heap-
; Tip 3: The solution is a one-liner
(: mem-sweep
  (All [T]
    (->
      ; heap
      (heap T)
      ; set of handles to keep
      (Setof handle)
      ; the new heap
      (heap T)
    )
  )
)
;; Mem-sweep func uses heap-filter to create new heap that consists only key-value pairs with keys present in to-keep.
;; Lambda func takes input k and v and returns true if k is member of to-keep, or else false.
;; Returns if k is member of to-keep set, corresponds to k ∈ to-keep, using set-member?
(define (mem-sweep mem to-keep)
  (heap-filter (lambda (k v) (set-member? to-keep k)) mem)
)

;;;;;;;;;;;;;;;
;; Exercise 4

(: eff-map
  (All [State Input Output]
    (->
      (-> Input Output)
      (Listof (eff-op State Input))
      (eff-op State (Listof Output))
    )
  )
)
(define (eff-map f l)
  (error "todo")
)

;;;;;;;;;;;;;;;
;; Exercise 5

(: eff-exists?
  (All [State T]
    (->
      (-> T Boolean)
      (Listof (eff-op State T))
      (eff-op State Boolean)
    )
  )
)
(define (eff-exists? f l)
  (error "todo")
)

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
If we assume that the reference count algorithm is faulty and the reference count overflows resetting back to zero,
it's possible that the correctness and the completeness of memory management, in freeing memory that is no longer used, may be affected.
The memory manager could incorrectly release memory, that's still in use if the reference count resets to zero.
Undefined behavior and improper program execution may often occur from this. Additionally, if the reference count often exceeds,
the memory management can fail to release memory that is no longer needed, which could result in memory leaks and reduce completeness.
In conclusion, an overflowing reference count can undermine the soundness and completeness of the memory management system and endanger its integrity.
|#
