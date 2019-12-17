(ns day2)

(defn parse-int [i] (Integer/parseInt i))

(defn parse [file]
  (->> file
       clojure.java.io/resource
       slurp
       clojure.string/trim
       (#(clojure.string/split % #","))
       (map parse-int)
       vec))

(def numbers (delay (parse "input2.txt")))

(defn next-instruction [instructions pointer]
  (nth instructions (swap! pointer inc) nil))

(defn read-next-arg [instructions pointer parameter-mode]
  (let [next (next-instruction instructions pointer)]
    (if (= 0 parameter-mode)
      (nth instructions next)
      next)))

(defn write-result [instructions pointer val]
  (let [write-loc (next-instruction instructions pointer)]
    (assoc instructions write-loc val)))

(defmulti execute-code
  (fn [code instructions pointer _]
    (or code
        (next-instruction instructions pointer))))

(defmethod execute-code 1 [_ instructions pointer modes]
  (let [arg1 (read-next-arg instructions pointer (nth modes 0 0))
        arg2 (read-next-arg instructions pointer (nth modes 1 0))
        val (+ arg1 arg2)]
    (write-result instructions
                  pointer
                  val)))

(defmethod execute-code 2 [_ instructions pointer modes]
  (let [arg1 (read-next-arg instructions pointer (nth modes 0 0))
        arg2 (read-next-arg instructions pointer (nth modes 1 0))
        val (* arg1 arg2)]
    (write-result instructions
                  pointer
                  val)))

(defmethod execute-code 99 [_ _ _ _]
  nil)

(defmethod execute-code :default [_ instructions pointer _]
  (prn (str "Unknown code at " @pointer ": " (nth instructions @pointer)))
  instructions)

(defn int-code-runner
  ([instructions]
   (int-code-runner instructions (atom -1)))
  ([instructions pointer]
   (let [memory (execute-code nil instructions pointer [])]
     (if (nil? memory)
       instructions
       (int-code-runner memory pointer)))))

(defn run
  ([instructions]
   (-> instructions int-code-runner first))
  ([instructions noun verb]
   (let [instructions (-> instructions
                          (assoc 1 noun)
                          (assoc 2 verb))]
     (run instructions))))

(def answer1 (delay (run @numbers 12 2)))

(defn find-verb [input desired-result max-loop noun]
  (loop [verbs (range 0 max-loop)]
    (when-let [verb (first verbs)]
      (let [result (run input noun verb)]
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
