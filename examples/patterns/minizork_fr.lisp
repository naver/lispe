#!/usr/local/bin/lispe

; Actions on data structures
(defmacro belong (x l) (in l (keystr x)))

(data
   [Aller atom_]
   [Casser atom_ atom_]
   [Ouvrir atom_ atom_]
   [Tuer atom_ atom_]
   [Prendre atom_]
   [Lacher atom_]
)

; prendre un objet
(defpat action ( [Prendre x] )
   (ncheck (check_object position x)
      (println "Je ne peux pas prendre" (determinant x))
      (push belongings x)
      (println "J'ai ramassé" (determinant x))
   )
)

; Lâcher un objet
(defpat action ( [Lacher x] )
   (check (check_belongings x)
      (key objects (keystr position) x)
      (pop belongings (find belongings x))
      (println "J'ai déposé" (determinant x) " sur le sol")
      (display_position position)
   )
)

; casser une fenêtre avec un objet
(defpat action( [Casser 'fenetre x] )
   (ncheck (= position '(1 2))
      (println "Un examen de la vue s'impose. Il n'y a pas de fenêtre ici")
      (ncheck (and (check_belongings x) (eq x 'pierre))
         (println "Je ne vois pas comment casser une fenêtre avec" x)
         (update_direction (keystr position) "0:2")
         (key castlemap "1:2" "Vous vous tenez devant une fenêtre brisée")
         (println "La fenêtre est cassée")
      )
   )
)

; ouvrir la porte avec une clef
; It triggers new potential move to other positions
(defpat action ( [Ouvrir 'porte 'clef] )
   (ncheck (= position '(1 1))
      (println "Une porte ici??? Vraiment!!!")
      (ncheck (check_belongings 'clef)
         (println "")
         (update_direction (keystr position) "0:1")
         (update_direction "0:1" "0:0")
         (key castlemap "1:1" "Vous vous tenez devant un portail grand ouvert")
         (println "La porte s'ouvre")
      )
   )
)

; Tuer un ogre avec une épée
(defpat action ( [Tuer 'ogre x] )
   (ncheck (= position '(1 0))
      (println "Il n'y a pas d'ogre ici...")
      (ncheck (and (check_belongings x) (eq x 'glaive))
         (block
            (push dangers "1:0")
            (println "Cette arme n'est guère efficace. L'ogre vous attaque...")
         )
         (setg theend true)
         (println "Vous avez tué l'ogre. Il gardait un merveilleux trésor. Vous êtes riche au-delà de tous vos rêves. Félicitation!!!")
      )
   )
)

; Aller d'un lieu à l'autre
(defpat action ( [Aller direction] )
   (setq p (checkposition (zipwith '+ position (key directions direction))))
   (if (= p position)
      (println "Je ne puis me rendre à cet endroit")
      (block
         (setg position p)
         (display_position position)
      )
   )
)

; Action par défaut
(defpat action(_) (random_choice 1 msgs 10))

; Primitives
; Data for the game and basic instructions
; build the key string
(defun keystr(p)
   (+ (string (car p)) ":" (string (cadr p)))
)

; Vérifier la validité de ce chemin
(defun check_valid_path(p)
   (if (belong p (key moving (keystr position)))
      p
      position
   )
)

; Sommes nous toujours dans le jeu: 3x3 cases.
(defpat checkposition( [x y] )
   (check_valid_path
      (cond
         ((< x 0) (list 0 y))
         ((< y 0) (list x 0))
         ((> x 2) (list 2 y))
         ((> y 2) (list x 2))
         (true (list x y))
      )
   )
)

; check if the object is available on the ground and pick it up
; we also remove it from objects
; the last 'true' is actually a hack. pop returns the dictionary as output,
; if the dictionary is empty then it will be evaluated as nil in a 'if'

(defun check_object(p x)
   (setq k (keystr p))
   (cond ((eq (key objects k) x) (pop objects k) true))
)

; check if we own the object x
(defun check_belongings(x)
   (if (nullp (find belongings x))
      (block
         (println "Vous ne possédez pas de" x)
         nil
      )
      true
   )
)

; display a description of where we are at current position
(defun display_position(p)
   (setq k (keystr p))
   (println k (select (key castlemap k) "Rien à voir"))
   (check (key objects k)
      (println (+ "Il y a " (determinant (key objects k)) " sur le sol"))
   )
   (println "Vous possédez: " belongings)
)


; check if the position is a dangerous one
(defun check_danger (position)
   (belong position dangers)
)

; each direction is associated with some values to add position with
(setq directions { "nord": '(-1 0) "sud":'(1 0) "ouest":'(0 -1) "est":'(0 1)})

(defun determinant(x)
   (+
      (select
         (key article x)
         "de"
      )
      " "
      x
   )
)

(setq article {"pierre":"une" "glaive":"un" "clef":"une"})

; some synonyms
(setq synonyms
   {
      "Ouvre":"Ouvrir"
      "Ouvrez":"Ouvrir"
      "Relache":"Lacher"
      "Relacher":"Lacher"
      "Relachez":"Lacher"
      "Lache":"Lacher"
      "Lachez":"Lacher"
      "Depose":"Lacher"
      "Deposer":"Lacher"
      "Deposez":"Lacher"
      "Va":"Aller"
      "Allez":"Aller"
      "Marche":"Aller"
      "Marcher":"Aller"
      "Marchez":"Aller"
      "Deplacer":"Aller"
      "Deplacez":"Aller"
      "Deplace":"Aller"
      "Deplacez":"Aller"
      "Occire":"Tuer"
      "Execute":"Tuer"
      "Executez":"Tuer"
      "Executer":"Tuer"
      "Attaque":"Tuer"
      "Attaquer":"Tuer"
      "Attaquez":"Tuer"
      "Tue":"Tuer"
      "Tuez":"Tuer"
      "Casse":"Casser"
      "Cassez":"Casser"
      "Brise":"Casser"
      "Brisez":"Casser"
      "Briser":"Casser"
      "Prend":"Prendre"
      "Prends":"Prendre"
      "Prenez":"Prendre"
      "Saisir":"Prendre"
      "Saisis":"Prendre"
      "Saisissez":"Prendre"
      "Ramasse":"Prendre"
      "Ramassez":"Prendre"
      "Ramasser":"Prendre"
      "Recupere":"Prendre"
      "Recuperez":"Prendre"
      "Recuperer":"Prendre"
      "roc":"pierre"
      "rocher":"pierre"
      "galet":"pierre"
      "vitre":"fenetre"
      "glace":"fenetre"
      "portail":"porte"
      "cle":"clef"
      "gauche":"ouest"
      "droit":"est"
      "droite":"est"
      "devant":"nord"
      "derriere":"sud"
      "arriere":"sud"
      "epee":"glaive"
   }
)

(setq stopwords {
      "a":true
      "un":true
      "une":true
      "en":true
      "le":true
      "la":true
      "les":true
      "avec":true
      "sur":true
      "sol":true
      "terre":true
      "par":true
      "ses":true
      "son":true
      "sa":true
      "se":true
      "s":true
      "votre":true
      "ton":true
      "ta":true
      "de":true
      "des":true
      "au":true
      "l":true
      "toi":true
      "vous":true
      "vers":true
      "direction":true
   }
)


; update valid directions in both ways
(defun update_direction (current_position direction)
   (key moving current_position (cons direction (key moving current_position)))
   (cond
      ((key moving direction)
         (key moving direction (cons current_position (key moving direction)))
      )
      (true
         (key moving direction (cons current_position ()))
      )
   )
)

; these are the valid moves from one position to another
(setq moving (key))

(update_direction "1:1" "1:0")
(update_direction "1:1" "1:2")
(update_direction "1:1" "2:1")
(update_direction "1:2" "2:2")
(update_direction "2:1" "2:0")
(update_direction "2:1" "2:2")
(update_direction "2:0" "1:0")


; the description for each square, which contains something
(setq castlemap {
      "1:1":"Vous vous tenez devant une porte."
      "1:2":"Vous vous tenez devant une grande fenêtre"
      "1:0":"Vous vous tenez devant un Ogre monstrueux"
      "2:1":"Vous réveillez un serpent vénimeux. Il vous mord. La douleur est horrible..."
      "0:0":"Vous êtes dans une grande salle sombre"
      "0:2":"Vous êtes dans une petite pièce"
      "2:0":"Vous êtes au milieu d'une forêt"
      "2:2":"Vous êtes au milieu d'une grande plaine. Une rivière coule vers l'est"
   }
)

(setq objects {
      "1:1": 'pierre
      "0:2": 'clef
      "0:0": 'glaive
   }
)

(setq msgs '(
      "Je ne suis pas sûr de ce que vous voulez faire!!!"
      "Aucune idée de ce que je peux faire avec ça"
      "Désolé, je n'ai pas compris..."
      "Vous voulez faire quoi?"
      "Désolé, je me baladais. Vous disiez?"
      "Vous pouvez répéter la question?"
      "Par pitié... Ne redîtes pas cela..."
      "Bel essai... Ben non!!"
   )
)

; initialisation
(setq belongings '())
(setq position '(1 1))
(setq dangers '("2:1"))
(setq theend nil)
(setq commands '(commencement))

; We display our initial psoition
(display_position position)

(while (neq (car commands) 'Fin)
   (print "Vos ordres Monseigneur? ")
   (setq dialog (input))
   (if (eq dialog "fin")
      (block
         (println "Salut...")
         (break)
      )
   )

   (setq dialog (+ (upper (at dialog 0)) (deaccentuate (lower (extract dialog 1 (size dialog))))))
   (setq commands (map (\ (x) (select (key synonyms x) x)) (rgx_split (rgx "{%s%p}") dialog)))
   ; we transform each of our strings into atoms, for match sake
   ;we remove useless words: the, a etc..
   (setq commands (filter (\ (x) (not (key stopwords x))) (map 'atom commands)))

   ; our commands is now a list of atoms that should match a data structure
   ; if we cannot evaluate it, then we display a random error message
   (maybe
      (println (action commands))
      (println (random_choice 1 msgs 10))
   )

   (check (check_danger position)
      (println "Vous êtes mort!!!")
      (setq commands '(Fin))
   )

   (if theend
      (setq commands '(Fin))
   )
)

(print "Fin")






