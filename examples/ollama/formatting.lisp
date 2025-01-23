;Date: 23/01/2025
;Author: Claude Roux
;Description: Formating output


(setq blue (+ (chr 27) "[0;34;49m"))
(setq blue_bold (+ (chr 27) "[1;34;49m"))
(setq blue_italic (+ (chr 27) "[3;34;49m"))
(setq dull (+ (chr 27) "[0m"))
(setq red (+ (chr 27) "[1;31;49m"))
(setq red_italic (+ (chr 27) "[3;31;49m"))
(setq blue_in_grey (+ (chr 27) "[3;36;49m"))


(defun cleanstring(s)
   (setq r  (unique . rgx_findall (rgx "u%x%x%x%x") s))

   (if r
      (loop e r
         (setq s (replace s e . chr . integer . + "0x" (@@ e 1 0)))
      )
   )

   (setq s (replace s "%0A" "\n"))
   (setq s (replace s "%0C" "\n"))
   (setq s (replace s "%0D" "\r"))
   (setq s (replace s "%09" "\t"))
   (setq s (replace s "\\[\n" "["))
   (setq s (replace s "\n\\]" "]"))
   (setq s (replace s "\\[" "["))
   (setq s (replace s "\\]" "]"))
   (setq s (replace s "\\(" ")"))
   (setq s (replace s "\\)" ")"))
   s
)

(defun formatcontent(s)
   (setq s (cleanstring s))
   (setq iv (reverse (findall s "### ")))
   (loop i iv
      (set@@ s i " " blue_bold)
      (setq p (find s "\n" i))
      (if (neq p -1)
         (set@ s p (+ dull blue "\n"))
      )
   )

   (setq iv (reverse (findall s "## ")))
   (loop i iv
      (set@@ s i " " blue_bold)
      (setq p (find s "\n" i))
      (if (neq p -1)
         (set@ s p (+ dull blue "\n"))
      )
   )

   (setq sv (unique (rgx_findall (rgx "%*%*{%a%d%p }+%*%*") s)))
   (loop e sv
      (setq s (replace s e (+ blue_in_grey (@@ e 2 -2) dull blue)))
   )

   (setq sv (unique (rgx_findall (rgx "```{%a%d%p }+```") s)))
   (loop e sv
      (setq s (replace s e (+ blue_in_grey (@@ e 2 -2) dull blue)))
   )

   (setq iv (reverse (findall s "```")))
   (loop i (irange 0 (size iv) 2)
      (check (< (+ i 1) (size iv))
         (setq pb (@ iv i))
         (setq pf (@ iv (+ i 1)))
         (set@@ s pb (+ pb 3)  (+ dull blue))
         (set@@ s pf (+ pf 3)  (+ "\n" red_italic))
      )
   )

   (setq sv (unique (rgx_findall (rgx "`{%a%d%p }+`") s)))
   (loop e sv
      (setq s (replace s e (+ blue_in_grey (@@ e 1 -1) dull blue)))
   )
   (return s)
)

(defun prepare(msg)
   (setq msg (replace msg "\\n" "%0A"))
   (setq msg (replace msg "\n" "%0A"))
   (setq msg (replace msg "\r" "%0A"))
   (setq msg (replace msg "\t" "%09"))
   (return msg)
)



