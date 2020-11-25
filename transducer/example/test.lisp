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
(defpat traversing ((string_ (< 10 (size sentence))) result)
   (println (trim sentence))
)

; the fall back function, when nothing matches
(defpat traversing (_ _) nil)

(setq result ())
(traversing (json_read (+ _current "dialogue.json")) result)
(println (unique result))






