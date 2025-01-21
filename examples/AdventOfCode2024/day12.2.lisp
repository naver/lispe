(setq inp . maplist '(split _ "") . split (string (fread (+ _current "data/day12.txt"))) "\n")


(setq num_rows (size inp))
(setq num_cols (size (@ inp 0)))

(defun in_bounds(rc)
   (setq (r c)  rc)
   (and
      (>= r 0)
      (>= c 0)
      (< r sz)
      (< c sz)
   )
)

(defun get_plant(rc)
   (@ inp (@ rc 0) (@ rc 1))
)

(defun get_neighbors(rc)
   (setq (r c)  rc)
   (setq neighbors ())
   (setq ds '((-1 0) (0 1) (1 0) (0 -1)))
   (loop e ds
      (push neighbors (integers (+ r (@ e 0)) (+ c (@ e 1))))
   )
   (filterlist (\(n) (in_bound n)) neighbors)
)

(defun get_plant_neighbors(rc)
   (filterlist (\(n) (= (get_plan n) (get_plan rc))) (get_neighbors rc))
)

(defun get_region(rc)
   (setq visited (set))
   (setq region (set))
   (setq queue '(rc))
   (while queue
      (setq node (popfirst queue))
      (check (nullp (in visited node))
         (insert visited node)
         (insert region node)
         (setq neighbors (get_plant_neighbors node))
         (setq unvisited_neighbors (filterlist (\(n) (nullp (in visited n))) neighbors))
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
      (setq north_n (integers (- r 1) c))
      (setq nw_n (integers (- r 1) (- c 1)))
      (check (nullp (in region north_n))
         (setq same_edge (and (in region west_n) (null p (in region nw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

      (setq south_n (integers (+ r 1) c))
      (setq sw_n (integers (+ r 1) (- c 1)))

      (check (nullp (in region south_n))
         (setq same_edge (and (in region west_n) (null p (in region sw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

      (check (nullp (in region west_n))
         (setq same_edge (and (in region north_n) (null p (in region nw_n))))
         (if (nullp same_edge)
            (+= edges 1)
         )
      )

      (setq east_n (integers r (+ c 1)))
      (setq ne_n (integers (- r 1) (+ c 1)))

      (check (nullp (in region east_n))
         (setq same_edge (and (in region north_n) (null p (in region ne_n))))
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
;;
(setq total_price 0)
(loop region regions
   (setq plant (get_plant(next(iter(region)))
         area = len(region)
         edges = calc_edges(region)
         price = area * edges
         total_price += price
         # print(f'{plant} (area: {area}, edges: {edges}): {region}')

         print(total_price)
         ;;

