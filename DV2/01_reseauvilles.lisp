;;; list containing "our database"/records of the property lists
(defparameter *db-vol* '()
  "This is a global variable which contains VolInstances")

;;; This takes a list of the data and return a single record of the "vol" 
(defun make-vol (idVol VilleS VilleD Temps Distance)
  (list :idVol idVol :VilleS VilleS :VilleD VilleD :Temps Temps :Distance Distance))

;;; Add a "vol" record to the database
(defun add-vol (vol)
  (push vol *db-vol*))

;;; inserting the data (idVol VilleS VilleD Temps Distance)
(add-vol (make-vol 1 "toronto" "chicago" 1 550))
(add-vol (make-vol 2 "montreal" "paris" 8 4500))
(add-vol (make-vol 3 "londres" "paris" 1 500))
(add-vol (make-vol 4 "montreal" "new_york" 1 500))
(add-vol (make-vol 5 "chicago" "new_york" 2 700))
(add-vol (make-vol 6 "new_york" "tampa" 4 1200))
(add-vol (make-vol 7 "tampa" "cancun" 4 1100))
(add-vol (make-vol 8 "paris" "rome" 2 800))
(add-vol (make-vol 9 "toronto" "londres" 9 5300))
(add-vol (make-vol 10 "rome" "new_york" 8 4800))
(add-vol (make-vol 11 "toronto" "montreal" 1 500))

;;; making a way for us to query for the instences that have a certain ville Source
(defun select-by-villeS (ville)
  (remove-if-not
    #'(lambda (vol) (equal (getf vol :villeS) ville))
    *db-vol*))

;;; une fonction Lisp qui retourne les vols directs (ou indirects) 
;;; entre deux villes V1 et V2 since the question is asking for all
;;; the paths, then i'm assuming there would be no cycles in the graph
;;; makes a graph node just which contains the reference to the "vol" 
;;; and the path which we took to reach it
(defun make-node (vol villeS &optional path (duree 0) (distance 0))
  (list vol (cons villeS path) duree distance))

(defun get-actions (villeS &optional path (duree 0) (distance 0))
  (let ((action-nodes '())) 
    (dolist (vol (select-by-villeS VilleS))
      (push 
        (make-node 
          vol villeS 
          path 
          (+ duree (getf vol :Temps)) 
          (+ distance (getf vol :Distance))) 
        action-nodes))
    action-nodes))

;;; main function that finds paths between two gives citites
(defun find-path (VilleS VilleD)
  (do ((stack (get-actions VilleS)) (paths '())) 
    ((null stack) paths)
    (let ((node (pop stack)))
      (let ((reached-city (getf (car node) :villed)) 
          (cached-path (cadr node))
          (cached-duree (caddr node))
          (cached-distance (cadddr node)))
        (if (equal VilleD reached-city)
          (push 
            (list 
              (reverse (cons VilleD cached-path)) 
              cached-duree cached-distance ) 
            paths)
          (dolist 
            (n (get-actions 
                reached-city 
                cached-path 
                cached-duree 
                cached-distance))
            (push n stack)))))))