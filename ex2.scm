(define kakeizu
    (read
       (open-input-file "/usr/local/class/scheme/kakeizu")))

(define get-depth
  (lambda (ls depth)
    (cond ((null? ls) '() )
          ((= depth 1) (map car (cdr ls)))
          (else (apply append (map (lambda (ls) (get-depth ls (- depth 1))) (cdr ls))))
    )
))


(get-depth kakeizu 1)
(get-depth kakeizu 3)
(get-depth kakeizu 6)

(define search
  (lambda (ls name depth)
    (cond ((null? ls) 0)
          ((not (eq? (memq name ls) #f)) depth)
          (else (apply + (map (lambda (ls) (search ls name (+ depth 1))) (cdr ls))))
)))

(define get-cousin
  (lambda (ls name)
    (get-depth ls (apply + (map (lambda (ls) (search ls name 1)) (cdr ls))))
))

(get-cousin kakeizu '秀忠)
(get-cousin kakeizu '吉宗)
