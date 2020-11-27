(setq i (pipe))
(println "Init:" i)
(while i
    (println "pipe:" i)
    (println "rgx:" (rgx_findall (rgx "%d+") i))
    (setq i (pipe))
)






