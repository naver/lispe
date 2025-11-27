#!/usr/local/bin/lispe

; Actions on data structures
(defmacro belong (x l) (in l (keystr x)))
(defmacro remove(o k v) (pop (key@ o k) v))

(data@ (Move _) (Break _ _) (Open _ _) (Kill _ _) (Pick _ _) (Take _) (Drop _))

; pick up is the same as take with one more parameter
(defpat action ( (Pick 'up x))
   (action (Take x))
)

; take an object on the ground
(defpat action ( (Take x) )
   (if
      (check_object position x)
      (block
         (push belongings x)
         (println "Ok we have picked up:" x)
      )
      (println "Cannot pick up the:" x)
   )
)

; drop it at current position
(defpat action ( (Drop x))
   (if
      (check_belongings x)
      (block
         (key@ objects (keystr position) x)
         (pop belongings (find belongings x))
         (println "Ok! We have dropped" x " on the ground")
         (display_position position)
      )
   )
)

; break a window with an object: it has to be a stone
(defpat action( (Break w x))
   (if (check_object position w)
      (if (and
            (check_belongings x)
            (eq x 'stone))
         (block
            (update_direction (keystr position) "0:2")
            (key@ castlemap "1:2" "You are standing in front of a broken window")
            (println "Window is broken")
         )
         (println "Cannot break a window with that" x)
      )
      (println "Get a sight check. There is no window over here")
   )
)

; open door with a key
; It triggers new potential move to other positions
(defpat action ((Open 'door x))
   (if (= position '(1 1))
      (if (and (check_belongings x) (eq x 'key))
         (block
            (update_direction (keystr position) "0:1")
            (update_direction "0:1" "0:0")
            (key@ castlemap "1:1" "You are standing in front of an open gate")
            (println "The door opens")
         )
         (println "Cannot open a door with that" x)
      )
      (println "A door here??? Really!!!")
   )
)

; kill the ogre with a sword or you'll get into trouble
(defpat action ((Kill 'ogre x))
   (if (= position '(1 0))
      (if (and (check_belongings x) (eq x 'sword))
         (block
            (setg theend true)
            (println "You kill the ogre. He was guarding an wonderfull treasure. You are now rich beyond measure. Congratulation!!!")
         )
         (block
            (push dangers "1:0")
            (println "This is not a very efficient weapon. The ogre attacks you...")
         )
      )
      (println "There is no ogre here")
   )
)

; Moving from one position to another
(defpat action ( (Move 'north))
   (setq p (checkposition (- (car position) 1) (cadr position)))
   (if (= p position)
      (println "Cannot go in this direction")
      (block
         (setg position p)
         (display_position position)
      )
   )
)

(defpat action ( (Move 'south))
   (setg p (checkposition (+ (car position) 1) (cadr position)))
   (if (= p position)
      (println "Cannot go in this direction")
      (block
         (setg position p)
         (display_position position)
      )
   )
)

(defpat action ( (Move 'west))
   (setg p (checkposition (car position) (- (cadr position) 1)))
   (if (= p position)
      (println "Cannot go in this direction")
      (block
         (setg position p)
         (display_position position)
      )
   )
)

(defpat action ( (Move 'east))
   (setg p (checkposition (car position) (+ (cadr position) 1)))
   (if (= p position)
      (println "Cannot go in this direction")
      (block
         (setg position p)
         (display_position position)
      )
   )
)

; Default action
(defpat action(_) (random_choice 1 msgs 10))

; Data for the game and basic instructions
; build the key string
(defun keystr(p)
   (join p ":")
)

; check if a path is within the description in 'moving'
(defun check_valid_path(p)
   (if (belong p (key@ moving (keystr position)))
      p
      position
   )
)

; update valid directions in both ways
(defun update_direction (current_position direction)
   (key@ moving current_position (cons direction (key@ moving current_position)))
   (cond
      ((key@ moving direction)
         (key@ moving direction (cons current_position (key@ moving direction)))
      )
      (true
         (key@ moving direction (cons current_position ()))
      )
   )
)

; check if we are within the confines of the game: 3x3 square map.
(defun checkposition(x y)
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
   (check
      (in (key@ objects k) x)
      (remove objects k x)
      true
   )
)

; check if we own the object x
(defun check_belongings(x)
   (if (nullp (find belongings x))
      (block
         (println "You don't own a" x)
         nil
      )
      true
   )
)

; display a description of where we are at current position
(defun display_position(p)
   (setq k (keystr p))
   (println k (select (key@ castlemap k) "Nothing to see"))
   (println (if (key@ objects k) (+ "There is a " (key@ objects k)) ""))
   (println "You own: " belongings)
)

; check if the position is a dangerous one
(defun check_danger (position)
   (belong position dangers)
)

; some synonyms
(setq synonyms
   {"rock":"stone"
      "pebble":"stone"
      "Head":"Move"
      "Walk":"Move"
      "Go":"Move"
      "Attack":"Kill"
      "Slay":"Kill"
      "Slash": "Kill"
      "Smash":"Break"
      "Shatter":"Break"
      "Crash":"Break"
      "glass":"window"
      "Grab":"Take"
      "Get":"Take"
      "gate":"door"
      "left":"west"
      "right":"east"
      "forward":"north"
      "backward":"south"
      "back":"south"
   }
)

(setq stopwords {"to":true "a":true "the":true "with":true "your":true "his":true "her":true})


; these are the valid move from one position to another
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
      "1:1":"You are standing in front of a gate."
      "1:2":"You are standing in front of a large window"
      "1:0":"You are standing in front of a Ogre"
      "2:1":"You wake up an angry venominous snake. It bites you. The pain is terrible..."
      "0:0":"Your are in a large dark room"
      "0:2":"You are on in small room"
      "2:0":"You are in the middle of a forest"
      "2:2":"You are in a large plain. A river is flowing east"
   }
)

(setq objects {
      "1:1": '(stone door)
      "1:2": '(window)
      "0:2": '(key)
      "0:0": '(sword)
   }
)

; initialisation
(setq belongings '())
(setq position '(1 1))
(setq dangers '("2:1"))
(setq theend nil)
(setq commands '(commencement))

; We display our initial psoition
(display_position position)

(setq msgs '(
      "Not sure what you want to do!!!"
      "Do not know what to do here"
      "Sorry, I did not understand..."
      "You want to do what?"
      "Sorry, I took a walk. What did you say?"
      "Please... Don't say it again"
      "Nice try... But no"
   )
)

(while (neq (car commands) 'End)
   (print "Your order sire? ")
   (setq dialog (input@))
   (check (eq dialog "end")
      (println "Ok... Bye...")
      (break)
   )

   (setq dialog (lower dialog))
   (setq dialog (+ (upper (at dialog 0)) (extract dialog 1 (size dialog))))
   (setq commands (map (\ (x) (select (key@ synonyms x) x)) (split dialog " ")))
   ; we transform each of our strings into atoms, for match sake
   ;we remove useless words: the, a etc..
   (setq commands (filter (\ (x) (not (key@ stopwords x))) (map 'atom commands)))
   ; our commands is now a list of atoms that should match a data structure

   (maybe
      (println (action commands))
      (println (random_choice 1 msgs 10))
   )

   (check (check_danger position)
      (println "you are dead!!!")
      (setq theend true)
   )

   (if theend
      (setq commands '(End))
   )
)

(print "The end")


