; We load and traverse an xml document

(use 'lispe_xml)


(defun parcours(x id)
   (if (nullp x)
      nil
      (block
         (xml_setnodeid x id)
         (print "id: " (xml_nodeid x) " ")
         (if (eq (xml_name x) "text")
            (println (xml_content x))
            (println x (xml_line x) (xml_properties x))
         )
         (parcours (xml_child x) (+ 1 id))
         (parcours (xml_next x) (+ 1 id))
      )
   )
)


; We load our file first
; This instruction creates an xmldoc object
(setq doc (xml_load (+ _current "sentence.xml")))

; The document root is an xmlnode
(parcours (xml_root doc) 0)





