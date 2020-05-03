(defun unifier (ListDonnee ListPattern)
  (let ((donnee ListDonnee) (pattern ListPattern))
    (cond 
      ((and (null donnee) (null pattern))
        t)
      ((or (null donnee) (null pattern))
        nil)
      (t 
        (let ((p (pop pattern)))
          (if (symbolp p)
            (let ((matched '()) )
              (do ((d nil (pop donnee)) (first-iter T))
                ((and (null donnee) (null d)) nil)
                (if first-iter 
                  (setq first-iter nil) 
                  (push d matched))
                (let ((unified-symbols (unifier donnee pattern)))
                  (cond ((null unified-symbols)) ;;; do nothing / continue
                    ((equal T unified-symbols) 
                        (return (list (list p (reverse matched))))) 
                    ;;; If the recursive call of the "unifier" returned a result 
                    ;;; of the form ((X (1 2 3))) merge with it and return
                    (t 
                        (return (cons 
                                    (list p (reverse matched))
                                    unified-symbols)))))))
            (if (equal p (pop donnee))
              (unifier donnee pattern)
              nil)))))))