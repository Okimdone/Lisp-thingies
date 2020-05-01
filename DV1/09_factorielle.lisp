;;;factorielle d'un nombre.
(defun factorielle (n) 
    (if (zerop n) 
        1
        (* n (factorielle (- n 1)))))