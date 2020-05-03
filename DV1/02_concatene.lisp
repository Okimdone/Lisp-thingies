(defun concatene (L1 L2)
  (if (null L1) 
    L2
    (cons (car L1)
       (concatene (cdr L1) L2))))