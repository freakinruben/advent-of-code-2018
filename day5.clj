;;;; Part 1
;; https://adventofcode.com/2018/day/5

; (defn remove-last [a] (subs a 0 (-> a count dec)))

(defn polarity-matches [a b]
  (let [a (str a)
        b (str b)]
    (and (not= a b)
         (or (= (clojure.string/lower-case a) b)
             (= (clojure.string/lower-case b) a)))))

(defn add-or-drop [cur-char-arr next-char]
  (if (polarity-matches (last cur-char-arr) next-char)
    (pop cur-char-arr)
    (conj cur-char-arr next-char)))

(defn react-polymer [input]
  (let [input (char-array input)]
    (reduce add-or-drop
            [(first input)]
            (rest input))))

(def answer1 (-> "input5.txt" slurp react-polymer count))
; 11636


;;;; Part 2

(defn unique-units [polymer]
  (distinct (clojure.string/lower-case polymer)))

(defn filter-unit "Removes the given unit from the given polymer" [polymer unit]
  (let [upper-case-unit (Character/toUpperCase unit)]
    (remove #{unit upper-case-unit} polymer)))
;   (->> polymer
;        (filter #(not= unit (clojure.string/lower-case %1)))
;        clojure.string/join))

(defn remove-unit-and-react [polymer unit] (react-polymer (filter-unit polymer unit)))

(defn find-optimal-reaction [input]
  (let [units (unique-units input)
        input (char-array input)]
    (->> units
         (map #(remove-unit-and-react input %))
         (map count)
         (reduce min))))

; user=> (time (find-optimal-reaction input))
; "Elapsed time: 191354.11229 msecs"
; 5302