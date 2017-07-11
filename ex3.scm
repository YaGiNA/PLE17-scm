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

;(diff fx)

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

;(tangent 2 fx)

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

;(diff2 '(* y x) 'x)

(define delete-zero
  (lambda (lst)
    (cond
     ((equal? 0 (car lst)) (cdr lst))
     (else (cons (car lst) (delete-zero  (cdr lst)))))))

(define simple+
  (lambda (non-zero-list)
    (cond ((null? non-zero-list) 0)
          ((null? (cdr non-zero-list)) (car non-zero-list))
          (else (eval (cons '+ non-zero-list) (interaction-environment)))
      
)))

(define simple-
  (lambda (lst)
    (cond ((null? (delete-zero lst)) '-)
          (else (eval (cons '- (delete-zero lst)) (interaction-environment)))
)))

(define simple*
  (lambda (lst)
    (cond ((or (equal? (car lst) 0) (equal? (cadr lst) 0)) 0)
          ((equal? (car lst) 1) (cadr lst))
          ((equal? (car lst) 1) (cadr lst))
          (else (eval (cons '* lst) (interaction-environment)))
)))
    
(define simple
  (lambda (lst)
    (cond
      ((not(list? lst)) lst)
      ((equal? '+ (car lst)) (simple+ (delete-zero (cdr lst))))
      ((equal? '- (car lst)) (simple- (cdr lst)))
      ((equal? '* (car lst)) (simple* (cdr lst)))
      (else (map simple lst))
)))

(simple (diff '(* x 3)))