(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (if (null? rests)
    nil
    (cons (cons first (car rests)) (cons-all first (cdr rests)))
  )
)

(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) nil))
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s a)
    (if (null? s)
      nil
      (cons (cons a (cons (car s) nil)) (helper (cdr s) (+ a 1))))
  )
  (helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((or (< total 0) (null? denoms))
      nil)
    ;if the total is less than the first element, then the first element cannot be used, 
    ;so perform the list-change with the remainder of the list only.
    ((< total (car denoms))
      (list-change total (cdr denoms)))
    ;if the first element of denoms is equal to the total, then one list ends with that total, and
    ;another computation is needed for the remainder of the list of denoms. Append the results together
    ((= total (car denoms))
      (append 
        (list (list total))
        (list-change total (cdr denoms))
      ))
    ;otherwise, total is greater than first element, so make two recursive calls.
    ;first call including total, second call not including total
    (else
      (append
        (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
        (list-change total (cdr denoms))
      )
    )
  )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
    ((atom? expr)
     ; BEGIN PROBLEM 19
      expr
     ; END PROBLEM 19
    )
    ((quoted? expr)
     ; BEGIN PROBLEM 19
      expr
     ; END PROBLEM 19
    )
    ((or (lambda? expr) (define? expr))
      (let ((form (car expr))
         (params (cadr expr))
         (body (cddr expr)))
       ; BEGIN PROBLEM 19
       (append (list form params) (map let-to-lambda body))
       ; END PROBLEM 19
       )
    )
    ((let? expr)
     (let ((values (cadr expr))
         (body   (cddr expr)))
       ; BEGIN PROBLEM 19
       (define params (car (zip values)))
       (define args (map let-to-lambda (cadr (zip values))))
       (define body (map let-to-lambda body))
       (cons (append (list 'lambda params) body) args)
       ; END PROBLEM 19
       ))
    (else
     ; BEGIN PROBLEM 19
     (map let-to-lambda expr)
     ; END PROBLEM 19
     )))