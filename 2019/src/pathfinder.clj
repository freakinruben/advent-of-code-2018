(ns pathfinder
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

;
; INTERACTING WITH PARSED MAP
;

(defn get-tile [tiles [x y :as pos]]
  (and pos
       (some-> tiles (nth y nil) (nth x nil))))

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

;
; NEIGHBOURING TILES
;

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

;
; PARSING
;

(defn parse-tile [y x symbol]
  {:symbol symbol
   :pos [x y]})

(defn parse-line [line-nr line-str]
  (->> line-str
       (#(string/split % #""))
       (map-indexed (partial parse-tile line-nr))
       vec))

(defn parse-map [map-str]
  (->> map-str
       (#(string/split % #"\n"))
       (map-indexed parse-line)
       vec
       (update-tiles add-neighbours)))

(defn parse-file [file parse-fn]
  (->> file
       clojure.java.io/resource
       slurp
       parse-fn))

;
; DEBUG
;

(defn print-parsed-map [tiles highlighted-positions]
  (->> tiles
       (update-tiles #(if (highlighted-positions (:pos %2))
                        (assoc %2 :symbol "\033[1;31m*\033[0m")
                        %2))
       (map #(->> % (map :symbol) (string/join "")))
       (string/join "\n")
       println))

;
; PATHFINDING
;

(def iterations (atom 0))

(defn empty-queue? [_ {:keys [solutions step]}]
  (when (not step)
    (prn @iterations "finished queue!")
    {:solutions solutions}))

(defn run-halt? [_ {:keys [solutions queue]}]
  (when (= @iterations 1000000)
    (prn @iterations "halt! queue:" (count queue))
    {:solutions solutions}))

(defn run-has-better-solution? [_ {:keys [solutions step queue]}]
  (when (some->> solutions first (> (:steps step)))
    ; (prn "abort trial, better solution found..")
    {:queue queue :solutions solutions}))

(defn run-queue-neighbours-bfs [_ {:keys [queue solutions step tile]}]
  (let [{:keys [walked steps pos]} step
        walked (conj walked pos)
        neighbours (->> tile
                        :neighbours
                        (filter !nil?)
                        (filter (comp not walked))
                        (map #(do {:pos % :walked walked :steps (inc steps)})))]
    {:queue (concat neighbours queue)
     :solutions solutions}))
