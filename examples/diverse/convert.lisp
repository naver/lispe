(setq src (ls (+ _current "../../src")))

(loop apath src
   (setq r
      (rgx_findall
         (rgx `"~r+"`)
         (fread apath)
      )
   )
   (println r)
   (map 'size
      (filter
         (lambda (x)
            (> (size x) 0)
         )
         r
      )
   )
)






