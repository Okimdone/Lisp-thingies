;dernier élément d'une liste
(defun lastelem (L)
    (cond ((null L) '())
        ((equal 1 (length L)) (car L))
        (t (lastelem (cdr L)))))