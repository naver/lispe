; In order to load the lispe_transducer library with use,
; You must initialize LISPEPATH.
;
; LISPEPATH is an environment variable that must point
; to the directory where all libraries are stored
; Example:
; export LISPEPATH=/home/user/lispe/bin

(use 'lispe_transducer)

; We load our lexicon in a transducer object: english
(setq english (transducer (+ _current "english.tra")))

; This is a dictionary
; note $:z, which acts as a "rest of dictionary" operator
; z contains the rest of the dictionary
(defpat traversing({k:l $:z} result)
   (traversing l result)
   (traversing z result)
)

; This is a list
(defpat traversing ((x $ r) result)
   (traversing x result)
   (traversing r result)
)

; This is a string, we want all our preposition for sentences
; of at least 20 characters...
(defpat traversing ((string_ (< 20 (size sentence))) result)
   ;we parse our sentence with our lexicon
   (setq words (transducer_parse english sentence 2))
   (scanning words result)
)

; the fall back function, when nothing matches
(defpat traversing (_ _) nil)

; A preposition
(defun isprep (x)
   (loop e x
      (check (in e "+Prep")
         (return true)
      )
   )
)

; scanning all words from the parse list
(defun scanning(parse result)
   (loop x parse
      (check (isprep x)
         (push result (at x 0))
      )
   )
)

(setq result ())
(traversing (json_read (+ _current "dialogue.json")) result)
(println (unique result))




