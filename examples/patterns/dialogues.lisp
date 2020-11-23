

(defpat parcours({k:l $:z})
   (parcours l)
   (parcours z)
)

(defpat parcours ((x $ r))
   (parcours x)
   (parcours r)
)

(defpat parcours ((string_ (flip (in "restaurant" s))))
   (println 'resto s)
)

(defpat parcours ((string_ x))_)

(defpat parcours ((integer_ i)) _)

(defpat parcours ((number_ n)) _)

(defpat parcours (_) 'rien)

(parcours (json_read (+ _current "WOZ20648.json")))







