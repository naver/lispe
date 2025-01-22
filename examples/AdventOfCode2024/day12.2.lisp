(setq inp . maplist '(split _ "") . split (string (fread (+ _current "data/day12.txt"))) "\n")

(defmacro tas() (heap (\(a b) (select (>=< (@ a 0) (@ b 0)) (>=< (@ a 1) (@ b 1))))))
(setq num_rows (size inp))
(setq num_cols (size (@ inp 0)))

(defun in_bounds(rc)
   (setq (r c)  rc)
   (and
      (>= r 0)
      (>= c 0)
      (< r num_rows)
      (< c num_cols)
   )
)

(defun get_plant(rc)
   (@ inp (@ rc 0) (@ rc 1))
)

(defun get_neighbors(rc)
   (setq neighbors ())
   (setq ds '((-1 0) (0 1) (1 0) (0 -1)))
   (loop e ds
      (push neighbors (+ rc e))
   )
   (filterlist (\(n) (in_bounds n)) neighbors)
)

(defun get_plant_neighbors(rc)
   (filterlist (\(n) (= (get_plant n) (get_plant rc))) (get_neighbors rc))
)

(defun get_region(rc)
   (setq visited (tas))
   (setq region (tas))
   (setq queue (list rc))
   (while queue
      (setq node (car queue))
      (popfirst queue)
      (check (nullp (in visited node))
         (insert visited node)
         (insert region node)
         (setq neighbors (get_plant_neighbors node))
         (setq unvisited_neighbors (filterlist (\(n) (not (in visited n))) neighbors))
         (extend queue unvisited_neighbors)
      )
   )
   region
)

(defun calc_edges(region)
   (setq edges 0)
   (loop e region
      (setq (r c) e)
      (setq north_n (integers (- r 1) c))
      (setq west_n (integers (- r 1) c))
      (setq nw_n (integers (- r 1) (- c 1)))

      (check (nullp (in region north_n))
         (setq same_edge (and (in region west_n) (nullp (in region nw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

      (setq south_n (integers (+ r 1) c))
      (setq sw_n (integers (+ r 1) (- c 1)))

      (check (nullp (in region south_n))
         (setq same_edge (and (in region west_n) (nullp (in region sw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

      (check (nullp (in region west_n))
         (setq same_edge (and (in region north_n) (nullp (in region nw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

      (setq east_n (integers r (+ c 1)))
      (setq ne_n (integers (- r 1) (+ c 1)))

      (check (nullp (in region east_n))
         (setq same_edge (and (in region north_n) (nullp (in region ne_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )
   )
   edges
)


(setq regions ())
(setq visited (set))
(loop r (irange 0 num_rows 1)
   (loop c (irange 0 num_cols 1)
      (setq rc (integers r c))
      (check (nullp (in visited rc))
         (setq region (get_region rc))
         (loop e region
            (insert visited e)
         )
         (push regions region)
      )
   )
)


(setq total_price 0)

(loop region regions
   (setq plant (get_plant (@ region 0)))
   (setq area (size region))
   (setq edges (calc_edges region))
   (setq price  (* area edges))
   (+= total_price price)
)

(print total_price)

