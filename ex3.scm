(define ** expt)
(define fx '(+ (** x 4) (* -2 (** x 2)) 9) )
(define diff
  (lambda (ls)
    (cond
          ((number? ls) 0)
          ((equal? 'x ls) 1)
          ((equal? '+ (car ls)) `(+ ,@(map diff (cdr ls))))
          ((equal? '- (car ls)) `(- ,@(map diff (cdr ls))))
          ((equal? '* (car ls)) 
            `(+ (* ,(car (cdr ls)) ,(diff (car (cddr ls)))) (* ,(diff (car (cdr ls))) ,(car (cddr ls))))
          )
          ((equal? '** (car ls)) 
            `(* ,(car (cddr ls)) (* ,(diff (car (cdr ls))) (** ,(car (cdr ls)) (- ,(car (cddr ls)) 1))))
          )
          (else (map diff ls))
)))

;;(diff fx)

(define tangent-a
  (lambda (a fa)
    ((eval `(lambda (x) ,(diff fa)) (interaction-environment)) a)
))
(define tangent-fa
  (lambda (a fa)
    ((eval `(lambda (x) ,fa) (interaction-environment)) a)
))

(define tangent
  (lambda (a fa)
    `(+ (* ,(tangent-a a fa) x)
        ,(let ((fda (tangent-a a fa)) (fa_val (tangent-fa a fa)))
          (- fa_val (* fda a))
        )
)))

;;(tangent 2 fx)

(define diff2
  (lambda (ls d)
    (cond
          ((equal? ls d) 1)
          ((or (number? ls) (not(list? ls))) 0)
          ((equal? '+ (car ls)) `(+ ,@(map (lambda (ls) (diff2 ls d)) (cdr ls))))
          ((equal? '- (car ls)) `(- ,@(map (lambda (ls) (diff2 ls d)) (cdr ls))))
          ((equal? '* (car ls)) 
            `(+ (* ,(car (cdr ls)) ,(diff2 (car (cddr ls)) d)) (* ,(diff2 (car (cdr ls)) d) ,(car (cddr ls))))
          )
          ((equal? '** (car ls)) 
            `(* ,(car (cddr ls)) (* ,(diff2 (car (cdr ls)) d) (** ,(car (cdr ls)) (- ,(car (cddr ls)) 1))))
          )
          (else (map diff2 ls d))
)))

;;(diff2 '(* y x) 'x)

(define delete-zero
  (lambda (lst)
    (cond
     ((equal? '(0 0) lst) '())
     ((equal? 0 (car lst)) (cdr lst))
     ((equal? 0 (cadr lst)) (car lst))
     (else lst)
)))

(define simple+
  (lambda (lst)
    (cond ((list? (car lst)) (append (simple (car lst)) (cadr lst)))
          ((list? (cadr lst)) (append (car lst) (simple (cadr lst))))
          ((null? (delete-zero lst)) 0)
          ((null? (cdr (delete-zero lst))) (car (delete-zero lst)))
          ((and (number? (car lst)) (number? (cadr lst))) (eval (cons '+ (delete-zero lst)) (interaction-environment)))
          (else (cons '+ (delete-zero lst)))
)))

(define simple-
  (lambda (lst)
    (cond ((list? (car lst)) (append (simple (car lst)) (cadr lst)))
          ((list? (cadr lst)) (append (car lst) (simple (cadr lst))))
          ((null? (delete-zero lst)) (car lst))
          ((and (number? (car lst)) (number? (cadr lst))) (eval (cons '- (delete-zero lst)) (interaction-environment)))
          (else (cons '- (delete-zero lst)))
)))

(define simple*
  (lambda (lst)
    (cond ((list? (car lst)) (append (simple (car lst)) (cadr lst)))
          ((list? (cadr lst)) (append (car lst) (simple (cadr lst))))
          ((or (equal? (car lst) 0) (equal? (cadr lst) 0)) 0)
          ((equal? (car lst) 1) (cadr lst))
          ((equal? (cadr lst) 1) (car lst))
          (else (cons '* lst))
)))

(define simple**
  (lambda (lst)
    (cond ((list? (car lst)) (append (simple (car lst)) (cadr lst)))
          ((list? (cadr lst)) (append (car lst) (simple (cadr lst))))
          ((equal? (cadr lst) 0) 1)
          ((equal? (cadr lst) 1) (car lst))
          (else (cons '** lst))
)))
    
(define simple
  (lambda (lst)
    (cond
      ((not(list? lst)) lst)
      ((equal? '+ (car lst)) (simple+ (cdr lst)))
      ((equal? '- (car lst)) (simple- (cdr lst)))
      ((equal? '* (car lst)) (simple* (cdr lst)))
      ((equal? '** (car lst)) (simple** (cdr lst)))
      (else (map simple lst))
)))

(simple (diff '(** x 3)))
(append '(2) '(x))