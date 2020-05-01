(defun substitue (e1 e2 L)
    (cond ((null L) 
                '())
          ((equal e1 (car L)) 
                (cons e2 (substitue e1 e2 (cdr L))))
          ((listp (car L))
                (cons (substitue e1 e2 (car L)) (substitue e1 e2 (cdr L))))
          (t 
                (cons (car L) (substitue e1 e2 (cdr L))))))