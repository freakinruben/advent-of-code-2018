(defn parse-claim [claim]
  (let [matches (re-find #"(#\d+) @ ((\d+),(\d+)): ((\d+)x(\d+))" claim)
        parsed {:id     (nth matches 1)
                :left   (Integer/parseInt (nth matches 3))
                :top    (Integer/parseInt (nth matches 4))
                :width  (Integer/parseInt (nth matches 6))
                :height (Integer/parseInt (nth matches 7))}]
    (assoc parsed
           :right  (+ (:left parsed) (:width parsed))
           :bottom (+ (:top parsed) (:height parsed))
           :size   (* (:width parsed) (:height parsed)))))

(def claims (->> "input-3.txt" slurp clojure.string/split-lines (map parse-claim)))

(def example (->> "input-3-example.txt" slurp clojure.string/split-lines (map parse-claim)))

(defn get-size [claims]
  (let [width  (->> claims (map :right) (apply max))
        height (->> claims (map :bottom) (apply max))]
    [width height]))

(defn generate-grid [[width height]]
  (for [x (range 0 width)
        y (range 0 height)]
    [x y]))

(defn claimed-location? [[x y] {:keys [left right bottom top id]}]
  (if (and (>= x left)
           (< x right)
           (>= y top)
           (< y bottom))
    id))
    ; 1 0))

; (defn count-claims-per-inch-debug [grid claims]
;   (for [location grid
;         :let [claims (->> claims
;                           (map (partial claimed-location? location))
;                           (filter #(-> % nil? not)))]]
;     [location claims]))

(defn count-claims-per-location [location claims]
  (loop [total 0
         claims claims]
    (if (= 2 total)
      1
      (if (empty? claims)
        0
        (let [claimed (claimed-location? location (first claims))]
          (recur (if claimed (inc total) total)
                 (rest claims)))))))

(defn claim-fabric [claims]
  (let [size (get-size claims)
        grid (generate-grid size)]
    (map #(count-claims-per-location % claims) grid)))

; (defn print-grid [grid]
;   (let [rows (->> grid (group-by #(-> % first second)) (sort-by key))
;         output (atom "")]
;     (doseq [[row cells] rows]
;       (do (swap! output str "\n")
;           (doseq [[cell claims] cells]
;             (condp = (count claims)
;               0 (swap! output str ".")
;               1 (swap! output str (-> claims first (subs 1)))
;               (swap! output str "X")))))
;     (println @output)))

(defn part1 [claims]
  (->> claims
       claim-fabric
       (reduce +)))

; (-> claims part1 time)
; "Elapsed time: 250709.05077 msecs"
; 101469



(defn overlaps? [claim1 claim2]
  (and (not= (:id claim1) (:id claim2))
       (> (:right claim1)  (:left claim2))
       (< (:left claim1)   (:right claim2))
       (< (:top claim1)    (:bottom claim2))
       (> (:bottom claim1) (:top claim2))))

(defn count-overlapping-claims [claim claims]
  (->> claims
       (map #(overlaps? claim %))
       (filter true?)
       count))

(defn part2 [all-claims]
  (let [claims (sort-by :size claims)]
    (loop [claims claims]
      (if (empty? claims)
        nil
        (let [claim (first claims)
              overlapping (count-overlapping-claims claim all-claims)]
          (if (= 0 overlapping)
            (prn claim)
            (recur (rest claims))))))))
