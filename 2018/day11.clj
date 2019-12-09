(defn rack-id [x] (+ x 10))

(defn power-level-rack [x y]
  (* y (rack-id x)))

(defn hundreds-digit [nr]
  (if (< nr 100)
    0
    (-> nr str reverse (nth 2) str Integer/parseInt)))

(defn power-level-cell [x y grid-serial-nr]
  (-> (power-level-rack x y)
      (+ grid-serial-nr)
      (* (rack-id x))
      hundreds-digit
      (- 5)))

(def m-power-level-cell (memoize power-level-cell))

; (defn make-grid
;   ([size serial-nr
;     (for [x (range 0 300)]
;       (for [y (range 0 300)])
;       [x y (m-power-level-cell [x y] serial-nr)])]))

; (defn make-grid [start-x start-y size serial-nr]
;   (->> (range 0 (* size size))
      ;  (map #(let [x])))

(defn make-grid [start-x start-y size serial-nr]
  (for [x (range start-x (+ start-x size))
        y (range start-y (+ start-y size))]
    (m-power-level-cell x y serial-nr)))
; (defn get-row [start size grid]
;   (->> grid (drop start) (take size)))

; (defn get-sub-grid [start-x start-y size grid]
;   (->> grid
;        (get-row start-x size)
;        (map #(get-row start-y size %))))

(defn third [x] (nth x 2))

(defn sum-row [row] (apply + row))

(defn calculate-square-power [sub-grid]
  (->> sub-grid
       (map #(map third %))
       (map sum-row)
       sum-row))

(defn find-most-powerful-square [square-size serial-nr]
  (prn "find square" square-size (* (- 300 square-size) (- 300 square-size)))
  (let [len (- 300 square-size)]
    (->> (range 0 (* len len))
         (pmap (fn [i]
                 (let [x (mod i len)
                       y (int (/ i len))]
                   [x y (apply + (make-grid x y square-size serial-nr))])))
                  ;  (apply + (make-grid x y square-size serial-nr)))))

                             ;(get-sub-grid x y square-size)
                            ;  calculate-square-power)])))
        ;  (apply max))))
        ;  (sort-by third >)
        ;  first)))
         (apply max-key third))))

(defn find-square [grid size]
  (let [result (time (find-most-powerful-square size grid))]
    (prn "square" size result)
    [size result]))

; (time (find-most-powerful-square (make-grid 300 7803) 3))
; "Elapsed time: 1815.856042 msecs"
; [20 51 31]

(defn find-most-powerful-square-size [serial-nr]
  ; (let [grid (make-grid serial-nr 300)]
  (->> (range 1 50)
        ;  reverse
       (map (partial find-square serial-nr))
       (apply max-key #(-> % second third))))
      ;  (apply max-key second)))
