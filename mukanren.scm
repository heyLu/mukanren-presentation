; mu-kanren

(define (print x)
  (format #t "; ~A\n" x))

;;

; the empty state: no substitutions and no variables created yet
;
; a pair of such values is often abreviated s/c (substitutions and counter)
(define empty-state '(() . 0))

; (car empty-state) => ()
; (cdr empty-state) => 0
; mind you, caaaar and cadadr exist as well
; (= (cadadr l) (car (cdr (car (cdr l)))))

;;

; creates a new *logic* variable
(define (var x) (vector x))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

; (var? (var 0)) => #t
; (var? (vector 0)) => #t   ; ! (this won't happen here, but it can in general)
; (var? 0) => #f

;;

; walk looks up the value of variable u in the substitution s
(define (walk u s) ; u - variable or value to look up
                   ; s - substitution (association list of variables to values or other variables)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (format #t "; (walk ~A s) => ~A\n" u pr)
    (if pr
        (walk (cdr pr) s)
        u)))

; (and #t #t) => #t
; (and 3 4) => 4          ; !
; (and #t #t #f) => #f
; (and #f 3 4) => #f

; (define al '((1 7) (1 2) (2 3) (3 4) (5 6)))
; (assp (lambda (key) (= key  3) al) => (3 4)
; (assp (...                  1) al) => (1 7) ; !
; (assp (...                  5) a1) => (5 6)
; (assp (...                 42) a1) => #f

; (define s1 `((,(var 0) . 5) (,(var 1) . 6) (,(var 2) . ,(var 1))))
; (walk (var 0) s1) => 5          ; value for variable 0
; (walk (var 1) s1) => 6          ;         ...        1
; (walk (var 2) s1) => 6          ; variable 2 references variable 1     ; !
; (walk (var 42) s1) => #(42)     ; variable 42 does not exist
; (walk 7 s1) => 7
; be careful ...:
; (walk (var 0) `((,(var 0) . ,(var 3)) (,(var 3) . ,(var 0))))

; extending the substitution s with a new binding: variable x has the value v
(define (ext-s x v s)
  `((,x . ,v) . ,s))

;;

; constructs a new goal: u and v must be equal (e.g. it must be possible to unify them)
(define (=== u v)
  (lambda (s/c) ; lambda_g - goal constructor
    (let ((s (unify u v (car s/c)))) ; unifies u and v, returning a substitution, or false
      (print s)
      (if s
          (unit `(,s . ,(cdr s/c))) ; 1-element list with state in it
          mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

; (unit s1) => (s1)

;;

; unifies u and v and returns a new substitution, or false if they can't be unified
(define (unify uv vv s)
  (let ((u (walk uv s))   ; lookup the value of u
        (v (walk vv s)))  ;         ...         v
    (format #t "; (unify ~A = ~A with ~A = ~A)\n" uv u vv v)
    (cond
     ((and (var? u) (var? v) (var=? u v)) s) ; are already unified?
     ((var? u) (ext-s u v s))                ; u "unifies with" v, (var? v) and v != u or v is anything
     ((var? v) (ext-s v u s))                ; v        ...     u
     ((and (pair? u) (pair? v))              ; unify all elements of list u with all elements of list v (pairwise)
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))            ; compare values of u and v

; pair? = has a head and a tail: (1 . 2) (1 2 3)

; (unify (var 0) (var 0) '()) => ()
; (unify (var 0) 5 '()) => ((#(0) . 5))
; (unify 5 (var 0) '()) => ((#(0) . 5))
; (unify (var 0) (var 1) '()) => ((#(0) . #(1)))
; ...
; (unify 3 3 '()) => ()
; (unify 3 3 `((,(var 0) . 5))) => ((#(0) . 5))
; (unify 3 4 '()) => #f

; ...

(define (call/fresh f)
  (lambda (s/c)          ; "state with counter"
    (let ((c (cdr s/c))) ; variable counter
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

; (car '(1 2 3)) => 1
; (cdr '(1 2 3)) => (2 3)

; (car '(1 . 2) => 1
; (cdr '(1 . 2) => 2
; (cdr '(1 2)) => (2) ; !

; ((call/fresh (lambda (a)
;                (call/fresh (lambda (b)
;                              (=== `(,a 2 3) `(1 ,b 3))))))
;  empty-state)

; disj and conj are *goal combinators*

(define (disj g1 g2)
  (lambda (s/c)
    (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2)
  (lambda (s/c)
    (bind (g1 s/c) g2)))

;; 4.1. finite depth-first search

; concatenates the states in $1 and $2
(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   (else (cons (car $1) ; first state of $1
               (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
   ((null? $) mzero)
   (else (mplus
          (g (car $)) ; is g consistent with (car $)
          (bind (cdr $) g)))))

; example

; ((call/fresh (lambda (a)
;                 (disj (=== a 1)
;                       (=== a 2))))
;  empty-state)

; ((call/fresh (lambda (a)
;                 (conj (=== a 1)
;                       (=== a 2))))
;  empty-state)

; ((call/fresh (lambda (a)
;                (call/fresh (lambda (b)
;                              (disj (conj (=== a 1) (=== b 2))
;                                    (conj (=== a 2) (=== b 1)))))))
;  empty-state)

; (define (fives x) (disj (=== x 5) (fives x)))
; eat-all-memory:
; ((call/fresh fives) empty-state)

;; 4.2. infinite streams

(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus ($1) $2)))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (bind ($) g)))
   (else (mplus (g (car $)) (bind (cdr $) g)))))

(define (fives x)
  (disj (=== x 5)
        ; construct a "lazy" goal, e.g. a goal returning an "immature stream"
        (lambda (s/c)
          (lambda () ((fives x) s/c)))))

; ((call/fresh fives) empty-state)
; => ((((#(0) . 5)) . 1) . #<procedure>)
; ((call/fresh (lambda (x) (=== x x))) empty-state)
; => ((() . 1))
; ((call/fresh (lambda (x) (call/fresh (lambda (y) (=== x y))))) empty-state)
; => ((((#(0) . #(1))) . 2))
; ((call/fresh (lambda (x) (call/fresh (lambda (y) (conj (=== x y) (=== y x)))))) empty-state)
; => ((((#(0) . #(1))) . 2))

;; 4.3. interleaving streams

(define (sixes x)
  (disj (=== x 6)
        (lambda (s/c) (lambda () ((sixes x) s/c)))))
(define fives-and-sixes
  (call/fresh (lambda (x) (disj (fives x) (sixes x)))))

(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (sevens x)
  (disj (=== x 7)
        (lambda (s/c) (lambda () ((sevens x) s/c)))))
(define fives-sixes-and-sevens
  (call/fresh (lambda (x)
                (disj (fives x)
                      (disj (sixes x)
                            (sevens x))))))

;; 5. user-level functionality

;; 5.1. recovering miniKanren's control operators

; macro for η-delay
(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

; ((call/fresh (lambda (x) (disj+ (fives x) (sixes x) (sevens x)))) empty-state)

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
  (syntax-rules ()
    ; (fresh () ...)
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0)
                   (fresh (x ...)
                          g0 g ...))))))

;; 5.2. from streams to lists

(define (pull $)
  (if (procedure? $)
      (pull ($))
      $))

(define (take-all $)
  (let (($ (pull $)))
    (cond
     ((null? $) '())
     (else (cons (car $) (take-all (cdr $)))))))

(define (take n $)
  (if (zero? n)
      '()
      (let (($ (pull $)))
        (cond
         ((null? $) '())
         (else (cons (car $) (take (- n 1) (cdr $))))))))

;; 5.3. recovering reification

(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))

(define (reify-state/1st-var s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v)
      (let ((n (reify-name (length s))))
        (cons `(,v . ,n) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else v))))

;; 5.4. recovering the interface to scheme

(define empty-state '(() . 0))
(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    ; for example `(run 3 (x) (=== x 5))`
    ((_ n (x ...) g0 g ...)
     (mK-reify (take n (call/empty-state
                        (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (x ...) g0 g ...)))))))

;; example: peano numbers

; Z = 0, (S x) = x + 1
; (S (S (S Z))) = 3

(define (peano^o x)
  (conde
   ((=== x 'Z))
   ((fresh (y)
           (=== x `(S ,y))
           (peano^o y)))))

; (run 5 (res) (peano^o res))
; => (Z (S Z) (S (S Z)) (S (S (S Z))) (S (S (S (S Z)))))
; (run* (q) (peano^o '(S (S Z))))
; => (_.0)
; (run* (q) (peano^o '(S (S oops))))
; => ()

; https://www.youtube.com/watch?v=7kPMFkNm2dw
(define (plus^o x y z)
  (conde
   ; Z + y = z
   ((=== x 'Z) (=== y z))
   ; (S x) + y = (S (x + y))
   ((fresh (sx sz)
           (=== x `(S ,sx))
           (=== z `(S ,sz))
           (plus^o sx y sz)))))

; (run* (res x y) (plus^o x y '(S (S Z))) (=== res (list x y)))
; => ((Z (S (S Z)))
;     ((S Z) (S Z))
;     ((S (S Z)) Z))
