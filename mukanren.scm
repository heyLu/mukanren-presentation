; mu-kanren

(define (print x)
  (format #t "~A\n" x))

(define empty-state '(() . 0))

(define (var x) (vector x))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

;;

(define (walk u s) ; u - variable or value to look up
                   ; s - substitution (association list of variables to values or other variables
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (format #t "~A\n" pr)
    (if pr
        (walk (cdr pr) s)
        u)))

(define (ext-s x v s)
  `((,x . ,v) . ,s))

;;

(define (=== u v)
  (lambda (s/c) ; lambda_g - goal constructor
    (let ((s (unify u v (car s/c))))
      (print s)
      (if s
          (unit `(,s . ,(cdr s/c)))
          mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

;;

(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) s) ; are already unified?
     ((var? u) (ext-s u v s))                ; u "unifies with" v
     ((var? v) (ext-s v u s))                ; v        ...     u
     ((and (pair? u) (pair? v))              ; unify all elements of list u with all elements of list v (pairwise)
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))            ; compare values of u and v

; ...

(define (call/fresh f)
  (lambda (s/c)          ; "state with counter"
    (let ((c (cdr s/c))) ; variable counter
      ((f (var c)) . `(,(car s/c) . ,(+ c 1))))))
