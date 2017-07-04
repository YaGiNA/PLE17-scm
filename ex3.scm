(define diff
  (lambda (ls)
    (cond ((list? ls) (map diff ls))
          ((number? ls) 0)
          ((equal? 'x ls) 1)
          ((or (equal? '+ ls) (equal? '- ls)) ls)
)))

(diff '(+ 10 x))