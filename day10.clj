(def r-line #"position=< ?(-?\d+),[ ]+(-?\d+)> velocity=< ?(-?\d+),[ ]+(-?\d+)>")

(defn make-coordinates [x y]
  [(Integer/parseInt x) (Integer/parseInt y)])

(defn parse-line [line]
  (let [parsed (re-find r-line line)]
    {:position (make-coordinates (nth parsed 1) (nth parsed 2))
     :velocity (make-coordinates (nth parsed 3) (nth parsed 4))}))

(defn parse [filename]
  (->> filename
       slurp
       clojure.string/split-lines
       (map parse-line)))

(def example (parse "input-10-example.txt"))
(def input   (parse "input-10.txt"))


(defn move-coordinates [coordinates]
  (map (fn [{:keys [position velocity] :as coordinate}]
         (-> coordinate
             (update-in [:position 0] + (first velocity))
             (update-in [:position 1] + (second velocity)))) coordinates))

(defn get-x [coordinate] (-> coordinate :position first))
(defn get-y [coordinate] (-> coordinate :position second))

(defn between? [val min max]
  (< min val max))

(defn get-edges
  "returns [left top right bottom] coordinates"
  [coordinates]
  (let [x (map get-x coordinates)
        y (map get-y coordinates)]
    [(->> x (apply min)) ;(max -50))
     (->> y (apply min)) ;(max 0))
     (->> x (apply max)) ;(min 50))
     (->> y (apply max))])) ;(min 100))]))

; (defn is-visible? [coordinate]
;   (and (-> coordinate get-x  (> 0))
;        (-> coordinate get-y (> 0))))

(defn draw-line [[min-x min-y max-x max-y] coordinates]
  (let [coordinates (group-by get-x coordinates)]
    (loop [x min-x
           line []]
      (if (> x max-x)
        (clojure.string/join "" line)
        (recur (inc x)
               (conj line (if (get coordinates x) "#" ".")))))))

(defn draw-sky [coordinates]
  (let [;coordinates (filter is-visible? coordinates)
        sky         (group-by get-y coordinates)
        edges       (get-edges coordinates)
        [min-x min-y max-x max-y] edges]
    (prn edges)
    (loop [y     min-y
           lines []]
      (if (> y max-y)
        (clojure.string/join "\n" lines)
        (recur (inc y)
               (->> (get sky y []) (draw-line edges) (conj lines)))))))

(defn visible? [edges]
  (and (< (- (nth edges 2) (nth edges 0)) 300)   ; horizontal
       (< (- (nth edges 3) (nth edges 1)) 100))) ; vertical

(defn draw-and-ask [coordinates]
  (loop [coordinates coordinates
         iteration   0]
    (let [visible coordinates ;(->> coordinates (filter #(-> % get-y (between? -10 100))) (filter #(-> % get-x (between? -150 150))))]
          edges   (get-edges coordinates)]
      (when (= 0 (mod iteration 500))
        (prn "iteration" iteration edges))
      (if (not (visible? edges))
        (recur (move-coordinates coordinates) (inc iteration))
        (do (prn "iteration" iteration edges)
            (println (draw-sky visible))
            (prn iteration "Continue?")
            (let [continue? (read-line)]
              (when (= "y" continue?)
                (recur (move-coordinates coordinates) (inc iteration)))))))))

; "iteration" 10333 [119 124 180 133]
; [119 124 180 133]
; ..##....#....#..######..#.......#........####.....##....#.....
; .#..#...#....#.......#..#.......#.......#....#...#..#...#.....
; #....#..#....#.......#..#.......#.......#.......#....#..#.....
; #....#..#....#......#...#.......#.......#.......#....#..#.....
; #....#..######.....#....#.......#.......#.......#....#..#.....
; ######..#....#....#.....#.......#.......#.......######..#.....
; #....#..#....#...#......#.......#.......#.......#....#..#.....
; #....#..#....#..#.......#.......#.......#.......#....#..#.....
; #....#..#....#..#.......#.......#.......#....#..#....#..#.....
; #....#..#....#..######..######..######...####...#....#..######
; 10333 "Continue?"
