(use 'lispe_transducer)

(setq tr (transducer (+ _current "english.tra")))

(println (transducer_flags))

(println (transducer_lookup tr "check"))

; These flags are actually binary values that we combine with the | operator
(println (transducer_lookup tr "chack" 1 (| a_first a_change)))

(println "------")
(println (transducer_parse tr "the boy is drinking some water"))
(println "------")
(println (transducer_parse tr "the boy is drinking some water" 1))
(println "------")
(println (transducer_parse tr "the boy is drinking some water" 2))
(println "------")
(println (transducer_parse tr "the boy is drinking some water" 3))











