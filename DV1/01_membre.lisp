(defun membre (e L) 
  (cond ((null L) nil)
     ((equal e (car L)) L)
     (t (membre e (cdr L)))))