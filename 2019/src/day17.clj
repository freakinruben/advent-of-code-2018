(ns day17
  (:require [day2 :refer [parse-file
                          run]]
            [day5]
            [day9]))

(def numbers (delay (parse-file "input17.txt")))

(def camera-output (-> @numbers run :output time delay))

(defn draw-camera-output [output]
  (->> output
       (map char)
       clojure.string/join
       print-str))

(defn draw-parsed-output [output]
  (->> output
       (map :symbol)
       clojure.string/join
       print-str))

(derive ::newline      ::symbol)
(derive ::scafold      ::symbol)
(derive ::intersection ::scafold)
(derive ::open-space   ::symbol)
(derive ::robot        ::symbol)
(derive ::robot-left   ::robot)
(derive ::robot-right  ::robot)
(derive ::robot-up     ::robot)
(derive ::robot-down   ::robot)

(defn translate-symbol [symbol-code]
  (let [symbol (char symbol-code)]
    (condp = symbol
      \#       {:symbol symbol :name ::scafold}
      \.       {:symbol symbol :name ::open-space}
      \newline {:symbol symbol :name ::newline}
      \>       {:symbol symbol :name ::robot-right}
      \<       {:symbol symbol :name ::robot-left}
      \^       {:symbol symbol :name ::robot-up}
      \v       {:symbol symbol :name ::robot-down})))

(defn add-location [width idx pixel]
  (assoc pixel
         :location idx
         :x (mod idx width)
         :y (-> idx (/ width) Math/floor int)))

(defn is-intersection? [width output pixel]
  (let [left  (-> pixel :location (- 1))
        right (-> pixel :location (+ 1))
        down  (-> pixel :location (- width))
        up    (-> pixel :location (+ width))]
    (and (some-> output (nth left nil)  :name (isa? ::scafold))
         (some-> output (nth right nil) :name (isa? ::scafold))
         (some-> output (nth up nil)    :name (isa? ::scafold))
         (some-> output (nth down nil)  :name (isa? ::scafold)))))

(defn parse-camera-output [camera-output]
  (let [img-width (inc (.indexOf camera-output (int \newline)))
        output (->> camera-output
                    (map translate-symbol)
                    (map-indexed (partial add-location img-width)))]
    (loop [to-check output
           new-output []]
      (if (seq to-check)
        (let [pixel (first to-check)]
          (if (is-intersection? img-width output pixel)
            (recur (rest to-check)
                   (conj new-output (assoc pixel :name ::intersection :symbol \0)))
            (recur (rest to-check)
                   (conj new-output pixel))))
        new-output))))

(def answer1 (->> @camera-output
                  parse-camera-output
                  (filter #(-> % :name (= ::intersection)))
                  (map #(* (:x %) (:y %)))
                  (reduce +)
                  time
                  delay))
