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

(def input (-> "input3.txt"
               clojure.java.io/resource
               slurp
               clojure.string/trim
               parse-input
               delay))

(defn draw-horizontal [drawing direction direction-fn]
  (loop [drawing drawing
         steps (second direction)]
    (if (> steps 0)
      (let [last-coordinate (or (last drawing) [0 0])]
        (recur (conj drawing [(-> last-coordinate first direction-fn)
                              (second last-coordinate)])
               (dec steps)))
      drawing)))

(defn draw-vertical [drawing direction direction-fn]
  (loop [drawing drawing
         steps (second direction)]
    (if (> steps 0)
      (let [last-coordinate (or (last drawing) [0 0])]
        (recur (conj drawing [(first last-coordinate)
                              (-> last-coordinate second direction-fn)])
               (dec steps)))
      drawing)))

(defmulti draw-line
  (fn [_ direction] (first direction)))

(defmethod draw-line "R" [drawing direction]
  (draw-horizontal drawing direction inc))

(defmethod draw-line "L" [drawing direction]
  (draw-horizontal drawing direction dec))

(defmethod draw-line "D" [drawing direction]
  (draw-vertical drawing direction inc))

(defmethod draw-line "U" [drawing direction]
  (draw-vertical drawing direction dec))

(defmethod draw-line :default [_ direction]
  (prn "Unknown direction:" direction))

(defn draw-wire [directions]
  (loop [drawing []
         directions directions]
    (if (seq directions)
      (recur (draw-line drawing (first directions))
             (rest directions))
      (set drawing))))

(defn manhatten-distance [coordinate-a coordinate-b]
  (+ (Math/abs (- (first coordinate-a) (first coordinate-b)))
     (Math/abs (- (second coordinate-a) (second coordinate-b)))))

(defn find-closest-intersection [input]
  (let [wire1 (draw-wire (first input))
        wire2 (draw-wire (second input))
        crossings (clojure.set/intersection wire1 wire2)]
    (->> crossings
         (map #(manhatten-distance % [0 0]))
         sort
         first)))

(def answer1 (-> @input find-closest-intersection delay))
