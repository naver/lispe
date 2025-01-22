(setq inp . maplist '(split _ "") . split (string (fread (+ _current "data/day12.txt"))) "\n")

(setq num_rows (size inp))
(setq num_cols (size (@ inp 0)))

(defmacro get_plant(rc)
   (@ inp (@ rc 0) (@ rc 1))
)


(defun in_bounds(rc)
   (setq (r c)  rc)
   (and
      (>= r 0)
      (>= c 0)
      (< r num_rows)
      (< c num_cols)
   )
)

(defun get_neighbors(rc)
   (setq neighbors ())
   (setq ds '((-1 0) (0 1) (1 0) (0 -1)))
   (loop e ds
      (push neighbors (+ rc e))
   )
   (filterlist (\(n) (in_bounds n)) neighbors)
)

(defmacro get_plant_neighbors(rc)
   (filterlist (\(n) (= (get_plant n) (get_plant rc))) (get_neighbors rc))
)

(defun get_region(rc)
   (setq visited (set))
   (setq region (set))
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
   (setq ds '((-1 0) (0 -1) (-1 -1) (1 0) (1 -1) (0 1) (-1 1)))
   (setq edges 0)
   (loop e region
      (setq corners ())
      (loop de ds
         (push corners (+ e de))
      )

      (setq (north_n west_n nw_n south_n sw_n east_n ne_n) corners)

      (check (nullp (in region north_n))
         (setq same_edge (and (in region west_n) (nullp (in region nw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

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

