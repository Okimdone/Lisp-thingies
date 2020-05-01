;;;le plus grand nombre dans une liste de nombres
(defun plusgrand (L) 
    (cond ((null L) nil)
        ((equal 1 (length L)) (car L))
        (t (let ((a (car L)) (b (plusgrand (cdr L))))
                (if (> a b) 
                    a
                    b)))))