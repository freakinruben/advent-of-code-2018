(def numbers
  (->> "input2.txt"
       clojure.java.io/resource
       slurp
       clojure.string/trim
       (#(clojure.string/split % #","))
       (map #(Integer/parseInt %))
       vec
       delay))

(defn next-instruction [int-codes pointer]
  (nth int-codes (swap! pointer inc) nil))

(defn read-next-arg [int-codes pointer]
  (nth int-codes
       (next-instruction int-codes pointer)))

(defn write-result [int-codes pointer result]
  (assoc int-codes
         (next-instruction int-codes pointer)
         result))

(defmulti execute-code
  (fn [int-codes pointer]
    (next-instruction int-codes pointer)))

(defmethod execute-code 1 [int-codes pointer]
  (write-result int-codes
                pointer
                (+ (read-next-arg int-codes pointer)
                   (read-next-arg int-codes pointer))))

(defmethod execute-code 2 [int-codes pointer]
  (write-result int-codes
                pointer
                (* (read-next-arg int-codes pointer)
                   (read-next-arg int-codes pointer))))

(defmethod execute-code 99 [_ _]
  nil)

(defmethod execute-code :default [int-codes pointer]
  (prn "Unknown code: " (nth int-codes @pointer))
  int-codes)

(defn int-code-runner
  ([int-codes]
   (int-code-runner int-codes (atom -1)))
  ([int-codes pointer]
   (let [memory (execute-code int-codes pointer)]
     (if (nil? memory)
       int-codes
       (int-code-runner memory pointer)))))

(defn run [input noun verb]
  (let [input (-> input
                  (assoc 1 noun)
                  (assoc 2 verb))
        memory (int-code-runner input)]
    (first memory)))

(def answer1 (run @numbers 12 2))

(defn find-verb [input desired-result max-loop noun]
  (loop [verbs (range 0 max-loop)]
    (when-let [verb (first verbs)]
      (let [result (run input noun verb)]
        ; (prn noun verb result)
        (if (= desired-result result)
          {:noun noun :verb (first verbs)}
          (recur (rest verbs)))))))

(defn find-noun-verb [input desired-result max-loop]
  (loop [nouns (range 0 max-loop)]
    (when-let [noun (first nouns)]
      (let [result (find-verb input desired-result max-loop noun)]
        (if (map? result)
          result
          (recur (rest nouns)))))))

(def answer2
  (delay
   (let [result (find-noun-verb @numbers 19690720 70)]
     (+ (* 100 (:noun result))
        (:verb result)))))
