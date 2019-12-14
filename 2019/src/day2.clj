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
  (prn "Unknown code: " (nth int-codes @pointer)))

(defn int-code-runner
  ([int-codes]
   (int-code-runner int-codes (atom -1)))
  ([int-codes pointer]
   (let [memory (execute-code int-codes pointer)]
     (if (nil? memory)
       int-codes
       (int-code-runner memory pointer)))))

(def answer1
  (let [input (-> @numbers
                  (assoc 1 12)
                  (assoc 2 2))
        memory (int-code-runner input)]
    (first memory)))
