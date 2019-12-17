(ns day2)

(defn parse-int [i] (Long/parseLong i))

(defn parse-codes [codes]
  (->> codes
       (#(clojure.string/split % #","))
       (map parse-int)
       (zipmap (range))))

(defn parse-file [file]
  (-> file
      clojure.java.io/resource
      slurp
      clojure.string/trim
      parse-codes))

(def numbers (delay (parse-file "input2.txt")))

; The computer's available memory should be much larger than the initial program. 
; Memory beyond the initial program starts with the value 0 and can be read or 
; written like any other memory. (It is invalid to try to access memory at a 
; negative address, though.)
(defn read-memory [{:keys [instructions]} location]
  (assert (>= location 0))
  (get instructions location 0))

(defn next-instruction [{:keys [pointer] :as config}]
  (swap! pointer inc)
  (read-memory config @pointer))

(defn read-next-arg [{:keys [relative-base] :as config} parameter-modes mode-pos]
  (let [next (next-instruction config)]
    (condp = (nth parameter-modes mode-pos 0)
      0 (read-memory config next)
      1 next
      2 (read-memory config (+ relative-base next)))))

; Parameters that an instruction writes to will never be in immediate mode.
(defn write-result [config val]
  (let [write-loc (next-instruction config)]
    (assoc-in config [:instructions write-loc] val)))

(defmulti execute-code
  (fn [config code _]
    (or code
        (next-instruction config))))

(defmethod execute-code 1 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val (+ arg1 arg2)]
    (write-result config val)))

(defmethod execute-code 2 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val (* arg1 arg2)]
    (write-result config val)))

(defmethod execute-code 99 [config _ _]
  (assoc config :halted true))

(defmethod execute-code :default [{:keys [instructions pointer] :as config} _ _]
  (prn (str "Unknown code at " @pointer ": " (nth instructions @pointer)))
  config)

;
; runner
; 

(defn memory-to-vec [memory]
  (->> memory (sort-by key) (map second)))

(defn int-code-runner [config]
  (let [{:keys [halted output] :as result} (execute-code config nil nil)]
    (if halted
      (do (when (seq output)
            (prn "output" output))
          (-> result :instructions memory-to-vec))
      (int-code-runner result))))

(defn run
  ([instructions]
   (run instructions nil))
  ([instructions input-val]
   (int-code-runner {:instructions  instructions
                     :pointer       (atom -1)
                     :relative-base 0
                     :output        []
                     :input         input-val}))
  ([instructions noun verb]
   (let [instructions (-> instructions
                          (assoc 1 noun)
                          (assoc 2 verb))]
     (run instructions))))

(def answer1 (-> @numbers (run 12 2) first delay))

(defn find-verb [input desired-result max-loop noun]
  (loop [verbs (range 0 max-loop)]
    (when-let [verb (first verbs)]
      (let [result (first (run input noun verb))]
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
