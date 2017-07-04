(define diff
  (lambda (ls)
    (cond
          ((number? ls) 0)
          ((equal? 'x ls) 1)
          ((equal? '+ (car ls)) `(+ ,@(map diff (cdr ls))))
          ((equal? '- (car ls)) `(- ,@(map diff (cdr ls))))
          ((equal? '* (car ls)) 
            `(+ (* ,@(car (cdr ls)) ,@(diff (cdr (cdr ls)))) (* ,@(diff (car (cdr ls))) ,@(cdr (cdr ls))))
          )
          (else (map diff ls))
)))

(diff '(* 10 x))