;;; Creating a function that would simplify the insertion
;;; of items a bit
(defun putprop (symbol value prop)
  (setf (get symbol prop) value)) 

;;; Creating a global variable in which we can store the 
;;; instences of personne, and voiture
(defparameter *personnes* nil) 

;;; setting the symbole Voiture as a plist as asked in the homework
(progn (putprop 'Voiture '() 'instances)
    (putprop 
      'Voiture 
      (lambda (refvoiture) 
        (if (< 
            (get refvoiture 'capacite)
            (let 
              ((personne (get refvoiture 'proprietaire)))
              (+ 
                1 ;;; le proprietaire de la voiture 
                (if (null (get personne 'pere)) 0 1)
                (if (null (get personne 'mere)) 0 1)
                (length (get personne 'liste-enfants)))))
          (progn (print "Votre famille dépasse en nombre la capacité de votre voiture, Vous devez renouveler votre voiture!")
            nil)
          T)) 
      'test_capacite)
    (putprop 
      'Voiture 
      (lambda (refvoiture)  
        (if (< 
            5
            (- 
              (get refvoiture 'annee-en-cours)
              (get refvoiture 'annee-achat)))
          (let ()
            (print "la voiture date de plus de 5 ans, il faut alors changer sa couleur.")
            (print "Entrer la couleur : ")
            (putprop refvoiture (read-line) 'couleur)
            nil)
          t)) 
      'test_viellesse)
) 

;;; function which creates a plist for the given symbol "ref" 
;;; and then adds it to the global list *personnes* 
(defun add-personne (ref date-naissance pere mere nationalite poids liste-enfants voiture)
  (putprop ref date-naissance 'date-naissance)
  (putprop ref pere 'pere)
  (putprop ref mere 'mere)
  (putprop ref nationalite 'nationalite)
  (putprop ref poids 'poids)
  (putprop ref liste-enfants 'liste-enfants)
  (putprop ref voiture 'voiture)
  (push ref *personnes*)
  ref)


;;; function which creates a plist for the given symbol "ref" 
;;; and then adds it to the list of instances associated to the symbol Voiture
(defun add-voiture (ref proprietaire couleur puissance carburant 
    matricule capacite annee-achat annee-en-cours)
  (putprop ref proprietaire 'proprietaire)
  (putprop ref couleur 'couleur)
  (putprop ref puissance 'puissance)
  (putprop ref carburant 'carburant)
  (putprop ref matricule 'matricule)
  (putprop ref capacite 'capacite)
  (putprop ref annee-achat 'annee-achat)
  (putprop ref annee-en-cours 'annee-en-cours)
  (push ref (get 'Voiture 'instances))
  ref
  )

;;; Adding an extra function which adds a "voiture" and its "proprietaire"'s details
;;; in one go
(defun add-voiture-and-personne (refvoiture proprietaire couleur puissance carburant 
    matricule capacite annee-achat annee-en-cours
    date-naissance pere mere nationalite poids liste-enfants)
  (add-voiture refvoiture proprietaire couleur puissance 
    carburant matricule capacite annee-achat annee-en-cours)
  (add-personne proprietaire date-naissance pere mere nationalite
    poids liste-enfants refvoiture)
  refvoiture)

;;; example of instances
;;; (add-personne 'p1 '(02 02 1997) "hassan" "najat" "marocain" 70 '("amine" "anas" "aya") 'v1)
;;; (add-voiture 'v1 'p1 "red" 113 "Essence" "IQE2E" 4 2017 2020)
(add-voiture-and-personne 'v1 'p1 "red" 113 "Essence" "IQE2E" 4 2017 2020
              '(02 02 1997) "hassan" "najat" "marocain" 70 '("amine" "anas" "aya"))
(add-voiture-and-personne 'v2 'p2 "red" 113 "Essence" "IQE2E" 6 2017 2020
              '(02 02 1997) "hassan" "najat" "marocain" 70 '("amine" "anas" "aya"))


;;; Définissez une fonction "nouveau-ne(personne, nom_bebe)": elle ajoute nom_bebe à la liste
(defun nouveau-ne (personne nom_bebe)
  (push nom_bebe (get personne 'liste-enfants))
  (funcall (get 'voiture 'test_capacite) (get personne 'voiture)))

;;; Définisser une autre fonction "nouvelle-annee(instVoiture, nouvAnnee)": 
;;; elle m-a-j l'année en cours de la voiture instVoiture et lui applique 
;;; ensuite le test de vieille
(defun nouvelle-annee (instVoiture nouvAnnee)
  (putprop instVoiture nouvAnnee 'annee-en-cours)
  (funcall (get 'voiture 'test_viellesse) instVoiture))