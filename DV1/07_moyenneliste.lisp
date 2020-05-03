;;;la moyenne d'une liste
(defun moyenneliste (L &optional sum len)
  (cond ((or (null sum) (null len)) 
      (moyenneliste L 0 0))
    ((null L) 
      (if (zerop len) 
        0 
        (/ sum len)))
    (t 
      (moyenneliste (cdr L) (+ sum (car L)) (1+ len)))))