(ns day16)

(defn parse-int [i] (Integer/parseInt i))

(def example1 {:in "12345678" :out "01029498"})
(def example2 {:in "80871224585914546619083218645595" :out "24176176"})
(def example3 {:in "19617804207202209144916044189917" :out "73745418"})
(def example4 {:in "69317163492948606335995924319873" :out "52432133"})

(def numbers
  (->> "input16.txt"
       clojure.java.io/resource
       slurp
       clojure.string/trim))

(defn parse-input [input]
  (->> (clojure.string/split input #"")
       (map parse-int)))

(defn keep-last-digit [input]
  (-> input
      str
      last
      str
      parse-int))

; While each element in the output array uses all of the same input array 
; elements, the actual repeating pattern to use depends on which output element 
; is being calculated.
; The base pattern is 0, 1, 0, -1. Then, repeat each value in the pattern a 
; number of times equal to the position in the output list being considered.
; When applying the pattern, skip the very first value exactly once. (In other 
; words, offset the whole pattern left by one.)

(defn make-pattern* [pattern position length]
  (loop [new-pattern []
         pattern-todo pattern]
    (if (< (count new-pattern)
           (+ length 1)) ; we will skip the first value so make sure the pattern is long enough
      (let [r (repeat (inc position) (first pattern-todo))]
        (recur (apply conj new-pattern r)
               (-> pattern-todo rest seq (or pattern))))
      (rest new-pattern)))) ; skip first value

(def make-pattern (memoize make-pattern*))

; Each element in the new list is built by multiplying every value in the input 
; list by a value in a repeating pattern and then adding up the results.
; Then, only the ones digit is kept: 38 becomes 8, -17 becomes 7, and so on.

(defn calc-fff-value [input position base-pattern]
  (let [pattern (make-pattern base-pattern position (count input))]
    (->> (mapv * input pattern)
         (apply +)
         keep-last-digit)))


(defn run-fff-phase [input pattern]
  (map-indexed (fn [idx _] (calc-fff-value input idx pattern)) input))

(def base-pattern [0 1 0 -1])

(defn run-fff [input-str total-phases]
  (loop [phase 0
         input (parse-input input-str)]
    (if (= total-phases phase)
      (-> input clojure.string/join (subs 0 8))
      (recur (inc phase)
             (run-fff-phase input base-pattern)))))
