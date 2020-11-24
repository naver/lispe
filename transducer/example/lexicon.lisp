(use 'lispe_transducer)

(setq tr (transducer))

(println (transducer_flags))

(transducer_load tr (+ _current "english.tra"))

(println (transducer_lookup tr "check"))

(println (transducer_lookup tr "chack" 1 a_change))

(println "------")
(println (transducer_parse tr "the boy is drinking some water"))
(println "------")
(println (transducer_parse tr "the boy is drinking some water" 1))
(println "------")
(println (transducer_parse tr "the boy is drinking some water" 2))
(println "------")
(println (transducer_parse tr "the boy is drinking some water" 3))










