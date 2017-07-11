(define ** expt)
(define fx '(+ (** x 3) (* -2 (** x 2)) 9) )
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
            `(* ,(car (cddr ls)) (* ,(diff (car (cdr ls))) (** ,(car (cdr ls)) ,(- (car (cddr ls)) 1))))
          )
          (else (map diff ls))
)))

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
    (let ((fda (tangent-a a fa)) (fa_val (tangent-fa a fa)))
      (+ fa_val (- (* x fda) (* fda a)))
)))

(tangent 2 fx)