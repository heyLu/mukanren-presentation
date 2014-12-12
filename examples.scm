; miniKanren support :)
(define === ==)
;       =/=

; john is male. jane is female.
; => ???

(define (female p)
  (conde
   ((=== p 'jane))
   ((=== p 'june))
   ((=== p 'sue))
   ((=== p 'gina))))

(define (male p)
  (conde
   ((=== p 'john))
   ((=== p 'sam))
   ((=== p 'george))))

; (run* (q) (female 'jane))
; (run* (q) (female q))
; (run* (q) (female 'alice)) => ?
; (run* (q) (male q))

;; (define male
;;   (lambda (p)
;;     (conde ...)))

; john is a child of sue
; childOf(john, sue).
; childOf(jane, sue).
; childOf(sue, george).
; ...
; *not* (directly) translatable to µKanren

; fresh, conde, ===

(define succeed (=== #f #f))
(define fail (=== #f #t))

;; (define (child-of x y) succeed)

;; (fresh (john jane sue george)
;;   (=== `(,john ,jane ,sue ,george) '(john jane sue george))

;;   (child-of john sue)
;;   (child-of jane sue)

;;   (child-of sue george))

(define (child-of x y)
  (conde
   ((=== x 'john) (=== y 'sue))
   ((=== x 'john) (=== y 'sam))

   ((=== x 'jane) (=== y 'sue))
   ((=== x 'jane) (=== y 'sam))

   ((=== x 'sue)  (=== y 'george))
   ((=== x 'sue)  (=== y 'gina))))

; (run* (q) (child-of q 'sue))
; (run* (q) (child-of 'sue q))
; (run* (q) (fresh (p c) (child-of c p) (=== q `(,c ,p))))

; If X is a child of Y then Y is a parent of X.

(define (parent x y)
  (child-of y x))

; If X is a child of Y and Y is male then Y is a father of X.

(define (mother x y)
  (conj
    (child-of y x)
    (female x)))

(define (father x y)
  (conj ; or (fresh () ...)
    (child-of y x) 
    (male x)))

; If X is male and Y is female then X is of opposite sex from Y.
; If X is male and Y is female then Y is of opposite sex from X.

(define (opposite-sex x y)
  (conde
   ((male x) (female y))
   ((female x) (male y))))

; If X is a father of Y and Y is a parent of Z then X is a grandfather of .Z

(define (grandma x z)
  (fresh (y)
    (mother x y)
    (parent y z)))

(define (grandpa x z)
  (fresh (y)
    (father x y)
    (parent y z)))

; (run* (q) (fresh (p) (grandma q p)))
; (run* (q) (grandma 'gina q))
; (run* (q) (grandma 'sue q))

; (run* (q) (fresh (p) (grandpa q p)))

;; a simple form of (explicit) negation
;; (define (not-child-of x y)
;;   (cond
;;    ((child-of x y) fail)
;;    (succeed)))

;; (define (not^o gc)
;;   (lambda (s/c)
;;     (let ((res (take-all (gc s/c))))
;;       (if (pair? res)
;;         (fail s/c)
;;         (succeed s/c)))))

(define (not-child-of x y)
  (fresh (u v)
    (child-of u v)
    (conde
     ;((=== u x) (=== v y) fail)
     ((=/= u x) succeed)
     ((=/= v y) succeed))
    ;(=/= u x)
    ;(=/= u y)
    ;(=/= v y)
    ))

(define (not-child-of2 x y)
  (conde
   ((symbolo x) (fresh (u)
                  (child-of x u)
                  (=/= u y)))
   ((symbolo y) (fresh (u)
                  (child-of u y)
                  (=/= u x)))))

; (run* (q) (not-child-of2 q 'sue))
; => (john
;     (_.0 (=/= ((_.0 john))))
;     jane
;     (_.0 (=/= ((_.0 jane))))
;     sue sue)

(define (not-child-of3 x y)
  (cond
   ((symbol? x) (fresh (u) (child-of x u) (=/= u y)))
   ((symbol? y) (fresh (u) (child-of u y) (=/= u x)))
   (else (fresh (u v) (child-of u v) (=/= u x) (=/= v y)))))

; (run* (q) (not-child-of3 'sue q))
; => ((_.0 (=/= ((_.0 george))))
;     (_.0 (=/= ((_.0 gina)))))
; (run* (q) (not-child-of3 q 'sue))
; => ((_.0 (=/= ((_.0 john))))
;     (_.0 (=/= ((_.0 jane)))))
; (run* (q) (fresh (x y) (not-child-of3 x y) (=== q `(,x ,y))))
; => (((_.0 _.1) (=/= ((_.0 john)) ((_.1 sue))))
;     ((_.0 _.1) (=/= ((_.0 john)) ((_.1 sam))))
;     ((_.0 _.1) (=/= ((_.0 jane)) ((_.1 sue))))
;     ((_.0 _.1) (=/= ((_.0 jane)) ((_.1 sam))))
;     ((_.0 _.1) (=/= ((_.0 sue)) ((_.1 george))))
;     ((_.0 _.1) (=/= ((_.0 sue)) ((_.1 gina)))))


