(ns day20
  (:require [clojure.string :as string]
            [pathfinder :refer [!nil?
                                empty-queue?
                                filter-tiles
                                is-path?
                                is-portal?
                                get-neighbours
                                get-tile
                                parse-file
                                print-parsed-map
                                run-halt?
                                run-has-better-solution?
                                run-queue-neighbours-bfs
                                set-tiles
                                some-tiles
                                update-tiles]]))

(defn set-edges [tiles]
  (let [edges (set (apply concat
                          (first tiles)
                          (last tiles)
                          (mapv first tiles)
                          (mapv last tiles)
                          []))] ; without empty vec the last tiles are missing
    (update-tiles (fn [_ tile]
                    (if (edges tile)
                      (assoc tile :edge? true)
                      tile))
                  tiles)))

(defn make-portal [tiles tile]
  (let [neighbours (get-neighbours tiles tile)
        related (filter is-portal? neighbours)
        portal-tiles (conj related tile)
        name (->> portal-tiles
                  (map :symbol)
                  sort
                  (string/join ""))]
    (assoc tile
           :neighbours (->> neighbours ; remove neighbours that are not paths
                            (filter is-path?)
                            (map :pos)
                            seq)
           :edge? (->> portal-tiles (map :edge?) (some true?))
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
               :edge?         (:edge? adjacent-portal)
               :jump-to       (:destination adjacent-portal)
               :neighbours    (->> (:neighbours tile)
                                   (remove #(= % (:pos adjacent-portal)))
                                   (#(conj % (:destination adjacent-portal)))
                                   (filter !nil?)))
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

(defn parse-map [map-str]
  (->> map-str
       pathfinder/parse-map
       set-edges
       make-portals))

;
; DEBUG
;

(defn print-dept-map [tiles walked depth]
  (println "dept" depth (count walked))
  (->> walked
       (filter #(-> % last (= depth)))
       (mapv drop-last)
       set
       (print-parsed-map tiles)))

;
; PATHFINDING
;

(defn find-portal [tiles portal-name]
  (let [tile (some-tiles #(= portal-name
                             (:linked-portal %))
                         tiles)]
    (:pos tile)))

(def iterations (atom 0))

(defn run-is-exit? [_ {:keys [exit queue solutions step]}]
  (when (= (:pos step) exit)
    {:queue queue
     :solutions (conj solutions (:steps step))}))

(defn walk [tiles]
  (reset! iterations 0)
  (let [entrance (find-portal tiles "AA")
        exit     (find-portal tiles "ZZ")]
    (loop [solutions  (sorted-set)
           queue      (list {:pos entrance
                             :walked #{}
                             :steps 0})]
      (swap! iterations inc)
      (let [step   (first queue)
            result (some #(% tiles {:exit exit
                                    :queue (rest queue)
                                    :solutions solutions
                                    :step step
                                    :tile (get-tile tiles (:pos step))})
                         [empty-queue?
                          run-halt?
                          run-is-exit?
                          run-has-better-solution?
                          run-queue-neighbours-bfs])]
        (if (-> result :queue nil?)
          (:solutions result)
          (recur (:solutions result) (:queue result)))))))

(defn find-shortest-path [tiles]
  ; (clojure.pprint/pprint tiles)
  (print-parsed-map tiles #{})
  (-> (walk tiles)
      time))

;
; PART1
;

(def example1 (delay (find-shortest-path (parse-file "example20a.txt")))) ; 23
(def example2 (delay (find-shortest-path (parse-file "example20b.txt")))) ; 58
(def answer1  (delay (find-shortest-path (parse-file "input20.txt")))) ; 638

;
; PART 2
;

(defn update-depth [tiles prev cur]
  (let [tile (get-tile tiles cur)
        prev-tile (get-tile tiles prev)
        depth (nth prev 2 0)
        depth (cond
                (not (:jump-to tile))       depth
                (not (:jump-to prev-tile))  depth
                (:edge? prev-tile)          (dec depth)
                :else                       (inc depth))]
    ; (prn (conj cur depth) tile)
    (assoc cur 2 depth)))

(defn depth-sort [a b]
  (cond
    (= a b) 0
    (> (-> a :pos last)
       (-> b :pos last)) 1
    :else -1))

(defn run-check-depth [_ {:keys [step] :as state}]
  (let [depth (-> step :pos (nth 2))]
    (when (or (< depth 0)
              (> depth 100))
      (select-keys state [:solutions :queue]))))

(defn run-queue-neighbours-weighted [tiles {:keys [queue solutions step tile]}]
  ; (prn tile)
  (let [{:keys [walked steps pos]} step
        walked (conj walked pos)
        neighbours (->> tile
                        :neighbours
                        (map #(update-depth tiles pos %))
                        (filter (comp not walked))
                        (map #(do {:pos % :walked walked :steps (inc steps)})))]
    {:queue (apply conj queue neighbours)
     :solutions solutions}))

(defn run-is-exit2? [_ {:keys [exit queue solutions step]}]
  (when (= (:pos step) exit)
    (prn @iterations "found solutions" (:steps step))
    (let [to-remove (filter #(>= (:steps %) (:steps step)) queue)
          queue (if (seq to-remove) (apply disj queue to-remove) queue)]
      {:queue queue
       :solutions (conj solutions (:steps step))})))

(defn walk-with-depths [tiles]
  (reset! iterations 0)
  (let [entrance (conj (find-portal tiles "AA") 0)
        exit     (conj (find-portal tiles "ZZ") 0)]
    (loop [solutions  (sorted-set)
           queue      (sorted-set-by depth-sort
                                     {:pos entrance
                                      :walked #{}
                                      :steps 0})]
      (swap! iterations inc)
      (let [step   (first queue)
            result (some #(% tiles {:exit exit
                                    :queue (disj queue step)
                                    :solutions solutions
                                    :step step
                                    :tile (get-tile tiles (:pos step))})
                         [empty-queue?
                          run-halt?
                          run-is-exit2?
                          run-has-better-solution?
                          run-check-depth
                          run-queue-neighbours-weighted])]
        (if (-> result :queue nil?)
          (do (:solutions result))
          (recur (:solutions result) (:queue result)))))))

(defn find-shortest-path-with-depths [tiles]
  (print-parsed-map tiles #{})
  ; (print-parsed-map tiles (->> tiles (filter-tiles :edge?) (map :pos) set))
  (-> (walk-with-depths tiles)
      time))

(def example3 (delay (find-shortest-path-with-depths (parse-file "example20c.txt")))) ; 58
(def answer2  (delay (find-shortest-path-with-depths (parse-file "input20.txt")))) ; 7844
