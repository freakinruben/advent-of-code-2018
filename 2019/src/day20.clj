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
       (map :pos)))

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
    (assoc tile :destination (:pos destination))))

(defn add-jumps
  "if a tile is neighbouring a portal, add the jump coordinates"
  [tiles tile]
  (if (is-path? tile)
    (let [adjacent-portal (->> (get-neighbours tiles tile)
                               (filter :destination)
                               first)]
      (if adjacent-portal
        (assoc tile
               :jump-to (:destination adjacent-portal)
               :linked-portal (:portal adjacent-portal))
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

(defn find-entrance [config])

(defn step-on-tile [config tile])

(defn walk [config]
  (let [entrance (find-entrance config)]))

(defn find-shortest-path [config]
  (clojure.pprint/pprint config)
  (print-parsed-map config #{})
  (-> (walk config)
      vec
      sort
      time))

;
; INPUT
;

(def example1 (delay (find-shortest-path (parse-file "example20a.txt")))) ; 132
(def example2 (delay (find-shortest-path (parse-file "example20b.txt")))) ; 132
