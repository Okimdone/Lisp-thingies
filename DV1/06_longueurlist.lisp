;;;longueur d'une liste
(defun longueurlist (L)
    (if (null L) 0
        (1+ (longueurlist (cdr L)))))