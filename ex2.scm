(define kakeizu
    (read
       (open-input-file "/usr/local/class/scheme/kakeizu")))

(define get-depth
  (lambda (ls depth)
    (cond ((null? ls) '() )
          ((= depth 1) (map car (cdr ls)))
          (else (map (lambda (ls) (get-depth ls (- depth 1))) (cdr ls)))
    )
))

(define get-top
  (lambda (kakeizu)
    (map car (cdr kakeizu))
))

(get-depth kakeizu 2)