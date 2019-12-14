(def numbers
  (->> "input2.txt"
       clojure.java.io/resource
       slurp
       clojure.string/trim
       (#(clojure.string/split % #","))
       (map #(Integer/parseInt %))
       vec
       delay))

(defn execute-code [int-codes pointer]
  (let [operator (nth int-codes pointer)
        pos1 (nth int-codes (+ pointer 1) nil)
        pos2 (nth int-codes (+ pointer 2) nil)
        pos3 (nth int-codes (+ pointer 3) nil)]
    (condp = operator
      1 (assoc int-codes pos3 (+ (nth int-codes pos1) (nth int-codes pos2)))
      2 (assoc int-codes pos3 (* (nth int-codes pos1) (nth int-codes pos2)))
      99 nil)))

(defn int-code-runner
  ([int-codes]
   (int-code-runner int-codes 0))
  ([int-codes pointer]
   (let [result (execute-code int-codes pointer)]
     (if (nil? result)
       int-codes
       (int-code-runner result (+ 4 pointer))))))

(def answer1
  (let [input (-> @numbers
                  (assoc 1 12)
                  (assoc 2 2))
        memory (int-code-runner input)]
    (first memory)))
