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
     :max-iters 20000 ; (* 50 (count map-str))
     :keys      (filter #(-> % :symbol is-symbol-key?) tiles)
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

(defn manhattan-distance [point1 point2]
  (+ (Math/abs (- (:x point1)
                  (:x point2)))
     (Math/abs (- (:y point1)
                  (:y point2)))))

(defn get-closest-key-distance
  "Calculates the manhatten distance to each key for the given tile. Keys that
   are already found will be ignored"
  [{:keys [keys]} tile unfound-keys]
  (->> keys
       (filter #(-> % :symbol unfound-keys)) ; remove found keys
      ;  (map #(dec (manhattan-distance % tile)))
       (map #(manhattan-distance % tile))
      ;  (map #(let [dis (manhattan-distance % tile)]
      ;          (prn [(:x tile) (:y tile)] "=>" [(:x %) (:y %)] (:symbol %) "=" dis)
      ;          dis))
       sort
       first))

(defn accessible-neighbours
  "returns neighbours that are accessible for walking"
  [{:keys [input-map] :as config} {:keys [idx previous unfound walked] :as state} pos]
  (->> (:neighbours pos)
      ;  (filter #(or (not (previous %))         ; don't go back
      ;               (is-new-key? state pos))) ; unless we found a key
       (filter #(-> % previous not))           ; don't go back
       (map #(get input-map % nil))           ; get neighbour data
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

(defn add-new-items [coll new-items]
  ; (let [new (filter #(= -1 (.indexOf coll %)) new-items)]
  (let [coll (remove (set new-items) coll)]
    (->> new-items
      ;  (remove (set coll))
         (concat coll)
         (sort-by :estimated-cost)
         doall)))

(defn walk [config start-pos]
  (loop [queue [{:idx (:idx start-pos)
                 :previous #{}
                 :walked 0
                 :unfound (->> config :keys (map :symbol) set)}]
         solutions #{}
        ;  skipped #{}
         iterations 0]
    (if (seq queue)
      (let [state      (first queue)
            pos        (-> config :input-map (get (:idx state)))
            new-key?   (is-new-key? state pos)
            state      (add-potential-key state pos)
            neighbours (accessible-neighbours config state pos)]

        ; (when new-key?
        ;   (prn "found key" (:symbol pos))
        ;   (prn state pos)
        ;   (println neighbours "\n"))

        (when (= (mod iterations 10000) 0)
          (prn iterations "intermediate; queue:" (count queue) solutions)
          (prn "first" state)
          (->> queue (sort-by :estimated-cost) first (prn "sorted first"))
          (prn "last" (last queue)))

        (cond
          (>= iterations (:max-iters config)) ; prevent searching too long
          (do (prn "abort; left in queue:" (count queue) "; iterations" iterations "/" (:max-iters config))
              (prn "unique positions" (->> queue (map :idx) frequencies))
              (print-parsed-map config (->> queue (map :idx) set))
              (println "\n first 3")
              (->> queue (take 3) (map prn) doall)
              (println "\n sorted first 3")
              (->> queue (sort-by :estimated-cost) (take 3) (map prn) doall)
              (println "\n last 3")
              (->> queue (take-last 3) (map prn) doall)
              (println "\n sorted last 3")
              (->> queue (sort-by :estimated-cost) (take-last 3) (map prn) doall)
              solutions)

          ; stop this path if there are shorter solutions already
          ; (solutions (:walked state))
          (some->> solutions sort first (>= (:walked state)))
          (recur (rest queue)
                 solutions
                ;  (conj skipped (dissoc state :walked))
                 (inc iterations))

          ; add solution if all unfound keys are found
          (-> state :unfound count (= 0))
          (do
            (prn iterations "found solution" state "; queue" (count queue))
            (recur (->> queue
                        rest
                        (filter #(< (:walked %) (:walked state)))) ; clean up queue with less efficient paths
                   (conj solutions (:walked state))
                  ;  skipped
                   (inc iterations)))

          ; stop at death end
          (not (seq neighbours))
          (recur (rest queue)
                 solutions
                ;  (conj skipped (dissoc state :walked))
                 (inc iterations))

          ; queue neighbours
          :else
          (let [queue     (rest queue)
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
                queue (add-new-items neighbours queue)
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
      (do (prn "finished!" iterations (:max-iters config))
          solutions))))

(defn find-shortest-path [config]
  (print-parsed-map config #{})
  (-> (walk config (find-entrance config))
      vec
      sort
      time))

(def answer1 (delay (find-shortest-path @input-map)))
