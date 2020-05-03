(defun renverse (L)
  (if (null L)
    '()
    (append (cdr L) (list (car L)))))