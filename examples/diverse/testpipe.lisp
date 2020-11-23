(setq i (input))
(while (pipe)
    (println "pipe:" i)
    (println "rgx:" (rgx_findall (rgx "%d+") i))
    (setq i (input))
)




