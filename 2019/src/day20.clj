(ns day20
  (:require [clojure.string :as string]))

(defn !nil? [x] (not= nil x))

;
; IDENTIFYING TILES
;

(defn tile-is? [symbol tile]
  (-> tile :symbol (= symbol)))

(def is-wall? (partial tile-is? "#"))
(def is-path? (partial tile-is? "."))
(def is-void? (partial tile-is? " "))

(defn is-accessible? [tile]
  (not (or (is-void? tile)
           (is-wall? tile))))

(defn is-portal? [tile]
  (let [c (-> tile :symbol (.charAt 0) int)]
    (and (>= c (int \A))
         (<= c (int \Z)))))

(defn get-tile [tiles [x y]]
  (some-> tiles (nth y nil) (nth x nil)))

;
; PARSING
;

(defn update-tiles [tile-fn tiles]
  (mapv (fn [row]
          (mapv (partial tile-fn tiles) row))
        tiles))

(defn filter-tiles [filter-fn tiles]
  (->> tiles
       (map #(filter filter-fn %))
       (apply concat)))

(defn some-tiles [some-fn tiles]
  (some (fn [coll]
          (some #(when (some-fn %) %) coll))
        tiles))

(defn set-tiles [new-tiles tiles]
  (reduce #(assoc-in %1 (-> %2 :pos reverse) %2)
          tiles
          new-tiles))

(defn get-neighbour-positions
  "gets the locations of neighbour tiles that are not walls"
  [tiles [x y]]
  (->> [[x (- y 1)] [x (+ y 1)] [(- x 1) y] [(+ x 1) y]]
       (map (partial get-tile tiles))
       (filter #(some-> % is-accessible?))
       (map :pos)
       seq))

(defn add-neighbours [tiles tile]
  (if (is-accessible? tile)
    (assoc tile :neighbours (get-neighbour-positions tiles (:pos tile)))
    tile))

(defn get-neighbours [tiles tile]
  (->> tile
       :neighbours
       (map (partial get-tile tiles))))

(defn make-portal [tiles tile]
  (let [neighbours (get-neighbours tiles tile)
        related (filter is-portal? neighbours)
        name (->> (conj related tile)
                  (map :symbol)
                  sort
                  (string/join ""))]
    (assoc tile
           :neighbours (->> neighbours ; remove neighbours that are not paths
                            (filter is-path?)
                            (map :pos)
                            seq)
           :portal name)))

(defn link-tile
  "Adds the location of the second portal to a portal"
  [portal-tiles tile]
  (let [destination (some #(when (and (not= % tile)
                                      (= (:portal tile) (:portal %)))
                             %)
                          portal-tiles)]
    ; jump to the first neighbour next to a portal
    (assoc tile :destination (-> destination :neighbours first))))

(defn add-jumps
  "if a tile is neighbouring a portal, add the jump coordinates as a neighbour"
  [tiles tile]
  (if (is-path? tile)
    (let [neighbours (get-neighbours tiles tile)
          adjacent-portal (->> neighbours
                               (filter :portal)
                               first)]
      (if adjacent-portal
        (assoc tile
               :linked-portal (:portal adjacent-portal)
               :neighbours    (->> (:neighbours tile)
                                   (remove #(= % (:pos adjacent-portal)))
                                   (#(conj % (:destination adjacent-portal)))))
        tile))
    tile))

(defn make-portals
  "1. Find tiles that are part of a portal and link them
   2. Find all tiles that border a portal
   3. Add jump codes to those tiles to indicate to which position to jump"
  [tiles]
  (let [portal-tiles   (->> tiles
                            (filter-tiles is-portal?)
                            (map (partial make-portal tiles))
                            (filter :neighbours))
        linked-portals (map (partial link-tile portal-tiles)
                            portal-tiles)]
    (->> tiles
         (set-tiles linked-portals)
         (update-tiles add-jumps))))

(defn parse-tile [y x symbol]
  {:symbol symbol
   :pos [x y]})

(defn parse-line [line-nr line-str]
  (->> line-str
       (#(string/split % #""))
       (map-indexed (partial parse-tile line-nr))
       vec))

(defn parse-map [map-str]
  (let [tiles (->> map-str
                   (#(string/split % #"\n"))
                   (map-indexed parse-line)
                   vec
                   (update-tiles add-neighbours)
                   make-portals)]
    {:tiles     tiles
     :max-iters 50000 ; (* 50 (count map-str))
     :portals   (filter-tiles :portal tiles)}))

(defn parse-file [file]
  (->> file
       clojure.java.io/resource
       slurp
       parse-map))

;
; DEBUG
;

(defn print-parsed-map [config highlighted-positions]
  (->> config
       :tiles
       (update-tiles #(if (highlighted-positions (:pos %2))
                        (assoc %2 :symbol "*")
                        %2))
       (map #(->> % (map :symbol) (string/join "")))
       (string/join "\n")
       println))

;
; PATHFINDING
;

(defn find-portal [{:keys [tiles portals]} portal-name]
  (some-tiles #(= portal-name
                  (:linked-portal %))
              tiles))

(defn walk [{:keys [tiles max-iters] :as config}]
  (let [entrance (find-portal config "AA")
        exit     (find-portal config "ZZ")]
    (loop [queue      (list {:pos (:pos entrance)
                             :walked #{}})
           solutions  (sorted-set)
           iterations 0]
      ; (prn iterations (count queue) (first queue))
      (if (seq queue)
        (let [{:keys [walked pos]} (first queue)
              tile (get-tile tiles pos)
              iterations (inc iterations)]
          (cond
            (= iterations max-iters)
            (do (prn "halt!")
                solutions)

            ; found-exit?
            (= tile exit)
            (recur (rest queue)
                   (conj solutions (count walked))
                   iterations)

            ; is-dead-end?
            (-> tile :neighbours count (= 1))
            (recur (rest queue) solutions iterations)

            ; lets the walk the neighbours
            :else
            (let [walked (conj walked pos)
                  neighbours (->> tile
                                  :neighbours
                                  (filter !nil?)
                                  (filter #(-> % walked not))
                                  (map #(do {:pos % :walked walked})))]
              (recur (concat neighbours (rest queue))
                     solutions
                     iterations))))
        (do (prn "solved in" iterations)
            solutions)))))

(defn find-shortest-path [config]
  ; (clojure.pprint/pprint config)
  (print-parsed-map config #{})
  (-> (walk config)
      time))

;
; INPUT
;

(def example1 (delay (find-shortest-path (parse-file "example20a.txt")))) ; 23
(def example2 (delay (find-shortest-path (parse-file "example20b.txt")))) ; 58
(def answer1  (delay (find-shortest-path (parse-file "input20.txt")))) ; ?
