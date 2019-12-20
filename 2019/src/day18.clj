(ns day18)

(defn !nil? [x] (not= nil x))

(defn is-symbol-key? [symbol]
  (let [c (int (.charAt symbol 0))]
    (and (>= c (int \a))
         (<= c (int \z)))))

(defn is-symbol-accesible? [symbol]
  (or (= "." symbol)
      (= "@" symbol)
      (is-symbol-key? symbol)))

(defn translate-tile [map-width idx pxl-str]
  {:symbol pxl-str
   :idx idx
   :open? (is-symbol-accesible? pxl-str)
   :x (mod idx map-width)
   :y (-> idx (/ map-width) Math/floor int)})

(defn is-wall? [px]
  (-> px :symbol (= "#")))

(defn get-neighbours
  "add the locations of neighbour tiles that are not walls"
  [tunnel-map map-width {:keys [idx]}]
  (->> [(- idx 1)          ; left
        (+ idx 1)          ; right
        (- idx map-width)  ; up
        (+ idx map-width)] ; down
       (map #(get tunnel-map % nil))
       (filter #(-> % is-wall? not))
       (map :idx)))

(defn add-neighbours [map-width tunnel-map]
  (reduce (fn [acc [_ tile]]
            (if (is-wall? tile)
              acc
              (assoc-in acc
                        [(:idx tile) :neighbours]
                        (get-neighbours acc map-width tile))))
          tunnel-map
          tunnel-map))

(defn close-dead-ends
  "updates the map when it finds tunnels that lead no-where"
  [map-width tunnel-map]
  (loop [todo (->> tunnel-map vals (map :idx) sort)
         output tunnel-map]
    (if (seq todo)
      (let [cur (->> todo first (get output))]
        ; (when (and (= 41 (:y cur)))
        ;   (prn cur (get-neighbours output map-width cur)))
        (if (or (is-wall? cur)
                (-> cur :open? not)
                (-> cur :symbol is-symbol-key?)
                (-> cur
                    (#(get-neighbours output map-width %))
                    count
                    (> 1)))
          (recur (rest todo) output)
          (recur (concat (rest todo) (get-neighbours output map-width cur))
                 (assoc output
                        (:idx cur)
                        (assoc cur :symbol "#" :is-open? false :neighbours nil)))))
      output)))

(defn manhattan-distance* [point1 point2]
  (+ (Math/abs (- (:x point1)
                  (:x point2)))
     (Math/abs (- (:y point1)
                  (:y point2)))))

(def manhattan-distance (memoize manhattan-distance*))

(defn add-distances-to-keys
  "calculates the distance for each key to all the other keys"
  [keys]
  (map (fn [key]
         (assoc key
                :distance-to
                (->> keys
                     (filter #(not= key %))
                     (reduce #(assoc %1
                                     (:symbol %2)
                                     (manhattan-distance key %2))
                             {}))))
       keys))

(defn parse-map [map-str]
  (let [map-width (.indexOf map-str "\n")
        tiles (->> map-str
                   (#(clojure.string/split % #""))
                   (filter #(not= % "\n"))
                   (map-indexed (partial translate-tile map-width)))]
    {:input-map (->> tiles
                     (reduce #(assoc %1 (:idx %2) %2) {})
                     (close-dead-ends map-width)
                     (add-neighbours map-width))
     :map-width map-width
     :max-iters 50000 ; (* 50 (count map-str))
     :keys      (->> tiles
                     (filter #(-> % :symbol is-symbol-key?))
                     add-distances-to-keys)
    ;  :keys #{"a" "b" "c"}
     }))

(defn parse-file [file]
  (->> file
       clojure.java.io/resource
       slurp
       clojure.string/trim
       parse-map))

(defn print-parsed-map [config highlighted-positions]
  (loop [out (transient [])
         todo (->> config :input-map vals (sort-by :idx))
         prev nil]
    (if (seq todo)
      (let [cur          (first todo)
            line-change? (not= (:y prev) (:y cur))
            symbol       (:symbol cur)
            symbol       (if (highlighted-positions (:idx cur)) "*" symbol)
            symbol       (if line-change?
                           (str "\n" symbol)
                           symbol)]
        (recur (conj! out symbol)
               (rest todo)
               cur))
      (->> out
           persistent!
           (clojure.string/join "")
           println))))

(def input-map (-> "input18.txt" parse-file time delay))

(defn find-symbol [{:keys [input-map]} symbol]
  (some #(when (-> % val :symbol (= symbol)) (val %)) input-map))

(defn find-entrance [config]
  (find-symbol config "@"))

(defn is-new-key? [{:keys [unfound]} {:keys [symbol] :as pos}]
  (and (is-symbol-key? symbol)
       (unfound symbol)))

(defn add-potential-key [state {:keys [idx symbol] :as pos}]
  (if (is-new-key? state pos)
    (-> state
        (update-in [:unfound] #(disj % symbol)) ; removes key from unfound
        (assoc :previous #{})) ; reset walking history
    (update state :previous #(conj % idx))))

(defn is-open? [{:keys [unfound] :as state} pos]
  (or (:open? pos)
      ; (let [door (-> pos :symbol clojure.string/lower-case)]
      ;   (when (unfound door) (prn "found locked door" door state))
      ;   (-> door unfound not))))
      (-> pos :symbol clojure.string/lower-case unfound not)))

(defn distance-to-other-keys [])

(defn get-closest-key-distance
  "Calculates the manhatten distance to each key for the given tile. Keys that
   are already found will be ignored"
  [{:keys [keys]} tile unfound]
  (let [unfound-keys (filter #(-> % :symbol unfound) keys)]
    (->> unfound-keys
         (map #(manhattan-distance % tile))
      ;  (map #(let [dis (manhattan-distance % tile)]
      ;          (prn [(:x tile) (:y tile)] "=>" [(:x %) (:y %)] (:symbol %) "=" dis)
      ;          dis))
         sort
         first)))

(defn accessible-neighbours
  "returns neighbours that are accessible for walking"
  [{:keys [input-map] :as config} {:keys [previous unfound walked] :as state} pos]
  (->> (:neighbours pos)
       (filter #(-> % previous not))           ; don't go back
       (map #(get input-map %))               ; get neighbour data
       (filter (partial is-open? state))       ; filter out closed doors
       (map #(let [goal-distance (or (get-closest-key-distance config % unfound)
                                     0)
                   walked (inc walked)]
              ;  (prn (count unfound) goal-distance walked)
               {:idx (:idx %)
                :previous previous; (conj previous idx)
                :goal-distance goal-distance
                :unfound unfound
                :walked walked
                :estimated-cost (* (count unfound) (+ goal-distance walked))})) ; the more keys we have, the higher the priority
      ;  (sort-by :estimated-cost)
       ))

; (defn add-new-items [coll new-items]
;   ; (let [new (filter #(= -1 (.indexOf coll %)) new-items)]
;   (let [coll (remove (set new-items) coll)]
;     (->> new-items
;       ;  (remove (set coll))
;          (concat coll)
;          (sort-by :estimated-cost)
;          doall)))

(defn estimated-cost-sort [x y]
  ; (prn "sort" x y (compare (:estimated-cost x) (:estimated-cost y)))
  (if (= x y)
    0
    (let [c (compare (:estimated-cost x) (:estimated-cost y))]
      (if (not= 0 c)
        c
        -1))))

(defn walk [config start-pos]
  (loop [queue (sorted-set-by estimated-cost-sort
                              {:idx (:idx start-pos)
                               :previous #{}
                               :walked 0
                               :unfound (->> config :keys (map :symbol) set)})
         solutions #{}
        ;  skipped #{}
         iterations 0]
    ; (prn iterations queue)
    (if (seq queue)
      (let [state      (first queue)
            queue      (disj queue state)
            pos        (-> config :input-map (get (:idx state)))
            new-key?   (is-new-key? state pos)
            orig-path  (:previous state)
            state      (add-potential-key state pos)
            neighbours (accessible-neighbours config state pos)]

        ; (println "\n" iterations "; queue" (count queue) "; path" orig-path)
        ; (when new-key?
        ;   (println "\tfound key" (:symbol pos)))
        ; (prn state pos)
        ;   ; (println neighbours "\n")
        ; (print-parsed-map config (->> orig-path set))

        ; (prn "queue:")
        ; (mapv prn (apply conj queue neighbours))
        ; (println "\n")

        (when (= (mod iterations 10000) 0)
          (prn iterations "intermediate; queue:" (count queue) solutions)
          (prn "first" state)
          (mapv prn neighbours)
          ; (->> queue (sort-by :estimated-cost) first (prn "sorted first"))
          (prn "last" (last queue)))

        (cond
          (>= iterations (:max-iters config)) ; prevent searching too long
          (do (prn "abort; left in queue:" (count queue) "; iterations" iterations "/" (:max-iters config))
              (prn "unique positions" (->> queue (map :idx) frequencies))
              (print-parsed-map config (->> queue (map :idx) set))
              (println "\n first 3")
              (->> queue (take 3) (map prn) doall)
              ; (println "\n sorted first 3")
              ; (->> queue (sort-by :estimated-cost) (take 3) (map prn) doall)
              (println "\n last 3")
              (->> queue (take-last 3) (map prn) doall)
              ; (println "\n sorted last 3")
              ; (->> queue (sort-by :estimated-cost) (take-last 3) (map prn) doall)
              solutions)

          ; stop this path if there are shorter solutions already
          ; (solutions (:walked state))
          (some->> solutions sort first (>= (:walked state)))
          (recur queue
                 solutions
                ;  (conj skipped (dissoc state :walked))
                 (inc iterations))

          ; add solution if all unfound keys are found
          (-> state :unfound count (= 0))
          (do
            (prn iterations "found solution" state "; queue" (count queue))
            (recur queue
                  ;  (->> queue
                  ;       (filter #(< (:walked %) (:walked state)))) ; clean up queue with less efficient paths
                   (conj solutions (:walked state))
                  ;  skipped
                   (inc iterations)))

          ; stop at death end
          (not (seq neighbours))
          (do ;(prn "dead end" state (:neighbours pos))
              ;(mapv prn (mapv #(get (:input-map config) %) (:neighbours pos)))
              ;(println "\n")
            (recur queue
                   solutions
                ;  (conj skipped (dissoc state :walked))
                   (inc iterations)))

          ; queue neighbours
          :else
          (let [;queue     queue
                ; new-items (map #(do {:idx      (:idx %)
                ;                      :previous (conj (:idx state))
                ;                      :walked   (-> state :walked inc)
                ;                      :unfound  (:unfound state)})
                ;                neighbours)

                ; queue (->> queue
                ;            (concat neighbours)
                ;            (sort-by :estimated-cost)
                ;            doall)

                ; new-items neighbours
                ; new-items (filter #(-> % (dissoc :walked) skipped not) new-items)
                ; queue (add-new-items neighbours queue)
                queue (apply conj queue neighbours)
                ; queue (add-new-items queue neighbours)
                ; queue (add-new-items queue new-items)
                ; new-set (set new-items)
                ; queue (->> new-items
                ;            reverse
                ;            (reduce #(cons %2 %1)
                ;                    (remove new-set queue))
                ;            doall)
                ; queue  (if new-key?
                ;          (add-new-items new-items queue)
                ;          (->> new-items
                ;               ; (filter #(= -1 (.indexOf queue %)))
                ;               (remove (set queue))
                ;               (concat queue)))
                ]
            (recur queue
                   solutions
                  ;  skipped
                   (inc iterations)))))
      (do (prn "finished!" iterations "/" (:max-iters config))
          solutions))))

(defn find-shortest-path [config]
  (print-parsed-map config #{})
  (-> (walk config (find-entrance config))
      vec
      sort
      time))

(def answer1 (delay (find-shortest-path @input-map)))
