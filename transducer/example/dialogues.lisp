; In order to load the lispe_transducer library with use,
; You must initialize LISPEPATH.
;
; LISPEPATH is an environment variable that must point
; to the directory where all libraries are stored
; Example:
; export LISPEPATH=/home/user/lispe/bin

(use 'lispe_transducer)

; We need a transducer object to load our lexicon
(setq english (transducer))

; We load our dictionary into our transducer
(transducer_load english (+ _current "english.tra"))

; This is a dictionary
(defpat traversing({k:l $:z} result)
   (traversing l result)
   (traversing z result)
)

; This is a list
(defpat traversing ((x $ r) result)
   (traversing x result)
   (traversing r result)
)

; This is a string, we want all our nouns
(defpat traversing ((string_ sentence) result)
   (check (size sentence)
      (setq words (transducer_parse english sentence 2))
      (scanning words result)
   )
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
(defpat scanning(parse result)
   (loop x parse
      (check (isprep x)
         (push result (at x 0))
      )
   )
)

; fall back
(defpat scanning(_ _) nil)

(setq result ())
(traversing (json_read (+ _current "dialogue.json")) result)
(println (unique result))




