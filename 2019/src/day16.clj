(ns day16)

; (set! *warn-on-reflection* true)

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

(defn last-digit [input]
  (-> input
      Math/abs
      (mod 10)))

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

(def get-indexes
  (memoize
   (fn [input-len]
     (prn "indexes" input-len)
     (range 0 input-len))))

; Each element in the new list is built by multiplying every value in the input 
; list by a value in a repeating pattern and then adding up the results.
; Then, only the ones digit is kept: 38 becomes 8, -17 becomes 7, and so on.

(defn calc-fff-value [position base-pattern input]
  (let [pattern    (make-pattern base-pattern position (count input))
        input      (drop position input)   ; remove 0s
        pattern    (drop position pattern) ; remove 0s
        pattern-it (clojure.lang.RT/iter pattern)
        input-it   (clojure.lang.RT/iter input)]
    (loop [result (int 0)]
      (if (and (.hasNext pattern-it) (.hasNext input-it))
        (recur (+ result
                  (* (.next pattern-it) (.next input-it))))
        (last-digit result)))))

(defn run-fff-phase [input pattern]
  (->> input
       count
       get-indexes
       (partition-all 2000)
       (pmap (fn [idxs] ;(println "idxs" idxs)
               (->> idxs
                    (mapv #(calc-fff-value % pattern input)))))
       (apply concat [])
       doall))

(def base-pattern [0 1 0 -1])

(defn run-fff [input-str total-phases]
  (loop [phase 0
         input (time (parse-input input-str))]
    ; (prn "begin phase" phase (count input) input)
    (if (= total-phases phase)
      (-> input clojure.string/join (subs 0 8))
      (recur (inc phase)
             (run-fff-phase input base-pattern)))))

(def answer1 (delay (time (run-fff numbers 100))))

; 
; Part 2
;

(defn calc-end [input-end]
  (loop [input-end input-end
         out []
         sum (reduce + input-end)]
    (if (empty? input-end)
      out
      (recur (rest input-end)
             (conj out (last-digit sum))
             (- sum (first input-end))))))

(def example5 "03036732577212944063491565474664")
(def example6 "02935109699940807407585447034323")
(def example7 "03081770884921959731165446850517")

(defn decode-signal [input]
  (let [offset  (Integer/parseInt (subs input 0 7))
        numbers (->> input
                     parse-input
                     (repeat 10000)
                     (apply concat)
                     time)]
    (prn "input len" (count numbers))
    (prn "offset   " offset)
    (->> (reduce (fn [acc _] (calc-end acc)) (drop offset numbers) (range 100))
         (take 8)
         clojure.string/join
         time)))

(def answer2 (delay (decode-signal numbers)))

