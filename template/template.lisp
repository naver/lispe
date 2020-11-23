# This program allows the creation of a minimal module to create an extension of LispE

(defun creation ()
   (print "New module name: ")
   (setq module_name (input))
   (setq makefile (fread (+ _current "Makefile")))
   (setq cxx (fread (+ _current "template.cxx")))
   (setq h (fread (+ _current "template.h")))
   (setq vc (fread (+ _current "template.vcxproj")))
   # We first create a directory with this name
   (command (+ "mkdir ../" module_name))
   (command (+ "mkdir ../" (+ module_name "/include")))
   (command (+ "mkdir ../" (+ module_name "/src")))

   (fwrite
      (+ "../"
         (+ module_name "/Makefile")
      )
      (replace makefile "%1" module_name)
   )


   (fwrite
      (+ "../"
         (+ module_name "/" module_name ".vcxproj")
      )
      (replace vc "%1" module_name)
   )

   (fwrite
      (+ "../"
         (+ module_name "/src/"
            (+ "lispe_" module_name ".cxx")
         )
      )
      (replace cxx "%1" module_name)
   )

   (fwrite
      (+ "../"
         (+ module_name "/include/"
            (+ "lispe_" module_name ".h")
         )
      ) (replace h "%1" module_name)
   )
)

(creation)





