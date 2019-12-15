(ns day3)

(defn parse-direction [input]
  [(subs input 0 1)
   (Integer/parseInt (subs input 1))])

(defn parse-wire [input]
  (->> input
       (#(clojure.string/split % #","))
       (map parse-direction)
       vec))

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (map parse-wire)
       vec))

(def example1 (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"))
(def example2 (parse-input "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
(def example3 (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4"))

(def input (-> "input3.txt"
               clojure.java.io/resource
               slurp
               clojure.string/trim
               parse-input
               delay))

;
; DRAWING
;

(defmulti draw-line
  (fn [_ direction] (first direction)))

(defmethod draw-line "R" [last-line direction]
  (let [[_ _ [x2 y2 :as p2]] last-line
        [_ steps] direction]
    [:hor p2 [(+ x2 steps) y2]]))

(defmethod draw-line "L" [last-line direction]
  (let [[_ _ [x2 y2 :as p2]] last-line
        [_ steps] direction]
    [:hor p2 [(- x2 steps) y2]]))

(defmethod draw-line "D" [last-line direction]
  (let [[_ _ [x2 y2 :as p2]] last-line
        [_ steps] direction]
    [:ver p2 [x2 (+ y2 steps)]]))

(defmethod draw-line "U" [last-line direction]
  (let [[_ _ [x2 y2 :as p2]] last-line
        [_ steps] direction]
    [:ver p2 [x2 (- y2 steps)]]))

(defmethod draw-line :default [_ direction]
  (prn "Unknown direction:" direction))

(defn draw-wire [directions]
  (loop [drawing []
         directions directions]
    (if (seq directions)
      (let [line (draw-line (or (last drawing) [nil nil [0 0]])
                            (first directions))]
        (recur (conj drawing line)
               (rest directions)))
      (->> drawing rest (group-by first))))) ; remove first point 0,0

;
; Intersect
;

(defn points-intersect [x1 x2 x3]
  (< x2 x1 x3))

(defn get-line-intersection
  [[_ [x1 y1] [x2 _]]   ; hor-line
   [_ [x3 y3] [_  y4]]] ; ver-line
  (let [[min-x max-x] (sort [x1 x2])
        [min-y max-y] (sort [y3 y4])]
    (when (and (<= min-x x3 max-x)
               (<= min-y y1 max-y))
      [x3 y1])))

(defn !nil? [x]
  (not= x nil))

(defn find-wire-intersection [line wire2]
  (->> wire2
       (map #(get-line-intersection line %))
       (filter !nil?)))

(defn find-wire-intersections [wire1 wire2]
  (let [a (->> wire1
               :hor
               (map #(find-wire-intersection % (:ver wire2)))
               (apply concat []))
        b (->> wire2
               :hor
               (map #(find-wire-intersection % (:ver wire1)))
               (apply concat []))]
    (concat a b)))

(defn manhatten-distance [coordinate-a coordinate-b]
  (+ (Math/abs (- (first coordinate-a) (first coordinate-b)))
     (Math/abs (- (second coordinate-a) (second coordinate-b)))))

(defn find-closest-intersection [input]
  (let [wire1 (draw-wire (first input))
        wire2 (draw-wire (second input))]
    (->> (find-wire-intersections wire1 wire2)
         (map #(manhatten-distance % [0 0]))
         sort
         first)))

(def answer1 (-> @input find-closest-intersection delay))
