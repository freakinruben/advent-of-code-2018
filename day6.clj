(def coordinates (->> "input-6.txt"
                      slurp
                      clojure.string/split-lines
                      (map #(clojure.string/split % #", "))
                      (map (fn [c] (map #(Integer/parseInt %) c)))
                      (sort-by first)))

(defn find-boundaries [coordinates]
  {:left (->> coordinates (map first) (apply min))
   :right (->> coordinates (map first) (apply max))
   :top (->> coordinates (map second) (apply min))
   :bottom (->> coordinates (map second) (apply max))})


(defn generate-locations [boundaries]
  (for [x (->> (range (:left boundaries) (inc (:right boundaries))))
        y (->> (range (:top boundaries) (inc (:bottom boundaries))))]
    [x y]))


(defn find-infinites
  "Loops over the edges of the map. If a coordinate is on the edge, its 
   considered infite and will be returned.
   Returns a set of infinite coordinates"
  [assigned boundaries]
  (let [top-row    (generate-locations (assoc boundaries :bottom (-> boundaries :top)))
        bottom-row (generate-locations (assoc boundaries :top    (-> boundaries :bottom)))
        left-row   (generate-locations (assoc boundaries :right  (-> boundaries :left)))
        right-row  (generate-locations (assoc boundaries :left   (-> boundaries :right)))
        edges      (reduce (partial apply conj) [top-row bottom-row left-row right-row])]
    (loop [infinite #{}
           edges edges]
      (if (empty? edges)
        infinite
        (recur (conj infinite (get assigned (first edges)))
               (rest edges))))))

(defn manhattan-distance [point1 point2]
  (+ (Math/abs (- (first point1)
                  (first point2)))
     (Math/abs (- (second point1)
                  (second point2)))))
;   (->> (zipmap point1 point2) ; {x1: x2, y1: y2}
;        (map (fn [[a b]] (Math/abs (- a b))))
;        (reduce +)))

(defn find-closest-coordinate [location coordinates]
  (let [distances (zipmap coordinates
                          (map #(manhattan-distance location %) coordinates))
        shortest (->> distances (map val) (reduce min))
        closest-by (filter #(= shortest (val %)) distances)]
    (when (= 1 (count closest-by))
      (-> closest-by first first))))

(defn find-largest-area [coordinates]
  (let [boundaries (find-boundaries coordinates)
        locations (generate-locations boundaries)
        assigned (reduce #(assoc %1 %2 (find-closest-coordinate %2 coordinates)) {} locations)
        infinites (find-infinites assigned boundaries)]
    (->> assigned
         vals
         (filter #(not (infinites  %))) ; remove infinte coordinates
         frequencies ; count each coordinate
         (sort-by val >) ; sort them so we can take the largest
         (take 3))))

; (def example [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])
; (def example-map {nil :. [1 1] :a [1 6] :b [8 3] :c [3 4] :d [5 5] :e [8 9] :f})

; (defn print-coordinates [coordinates]
;   (let [boundaries (find-boundaries coordinates)
;         locations (generate-locations boundaries)
;         mapped (map #(identity [% (find-closest-coordinate % coordinates)]) locations)
;         per-row (->> mapped (group-by #(-> % first second)) (sort-by key))
;         output (atom "")]
;     (doseq [[row cells] per-row]
;       (do (swap! output str "\n")
;           (doseq [[cell coordinate] cells]
;             (if (= cell coordinate)
;               (swap! output str (-> coordinate example-map name clojure.string/upper-case))
;               (swap! output str (-> coordinate example-map name))))))
;     (println @output)))
