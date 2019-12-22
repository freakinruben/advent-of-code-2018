(ns intcode)

;
; PARSER
;

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

;
; RUNNER
;

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

; Your ship computer already understands parameter mode 0, position mode, 
; which causes the parameter to be interpreted as a position - if the parameter 
; is 50, its value is the value stored at address 50 in memory. Until now, all 
; parameters have been in position mode.
; 
; Your ship computer will also need to handle parameters in mode 1, immediate 
; mode. In immediate mode, a parameter is interpreted as a value - if the 
; parameter is 50, its value is simply 50.

(defn read-next-arg [{:keys [relative-base] :as config} parameter-modes mode-pos]
  (let [next (next-instruction config)]
    (condp = (nth parameter-modes mode-pos 0)
      0 (read-memory config next)
      1 next
      2 (read-memory config (+ relative-base next)))))

; Parameters that an instruction writes to will never be in immediate mode.
(defn write-result [config val modes mode-pos]
  (assert (not= nil val))
  (let [next      (next-instruction config)
        write-loc (condp = (nth modes mode-pos 0)
                    0 next
                    1 next
                    2 (+ (:relative-base config) next))]
    ; (println "\twrite" next "->" write-loc "=>" val "; modes: " modes "; mode-pos" mode-pos)
    (assoc-in config [:instructions write-loc] val)))

; jump back 1 position to make sure the next execute reads from the correct location
(defn jump-pointer [config position]
  (reset! (:pointer config) (dec position)))

(defmulti execute-code
  (fn [_ code _] code))

(defmethod execute-code 1 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val (+ arg1 arg2)]
    ; (println "+" val "\n")
    (write-result config val modes 2)))

(defmethod execute-code 2 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val (* arg1 arg2)]
    ; (println "*" val "\n")
    (write-result config val modes 2)))

; Opcode 3 takes a single integer as input and saves it to the position given by 
; its only parameter. For example, the instruction 3,50 would take an input 
; value and store it at address 50.
; 
; Input can be either a single value or a list. In case of a list, it will read
; the first item from the list and then remove it from the available inputs
(defmethod execute-code 3 [{:keys [input] :as config} _ modes]
  (assert (not= nil input))
  (cond
    (coll? input)
    (-> config
        (write-result (first input) modes 0)
        (assoc :input (rest input))) ; remove read item from input

    (fn? input)
    nil

    :else
    (write-result config input modes 0)))

; Opcode 4 outputs the value of its only parameter. For example, the instruction 
; 4,50 would output the value at address 50.
(defmethod execute-code 4 [config _ modes]
  (let [val (read-next-arg config modes 0)]
    ; (println "output" val modes "\n")
    (update-in config [:output] #(conj % val))))

; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the 
; instruction pointer to the value from the second parameter. Otherwise, it does 
; nothing.
(defmethod execute-code 5 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)]
    ; (println "jump?" (> arg1 0) arg1 arg2 "\n")
    (when (> arg1 0)
      (jump-pointer config arg2))
    config))

; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the 
; instruction pointer to the value from the second parameter. Otherwise, it does 
; nothing.
(defmethod execute-code 6 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)]
    ; (println "jump?" (= arg1 0) arg1 arg2 "\n")
    (when (= arg1 0)
      (jump-pointer config arg2))
    config))

; Opcode 7 is less than: if the first parameter is less than the second 
; parameter, it stores 1 in the position given by the third parameter. 
; Otherwise, it stores 0.
(defmethod execute-code 7 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val  (if (< arg1 arg2) 1 0)]
    ; (println "<" val "\n")
    (write-result config val modes 2)))

; Opcode 8 is equals: if the first parameter is equal to the second parameter, 
; it stores 1 in the position given by the third parameter. Otherwise, it 
; stores 0.
(defmethod execute-code 8 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val  (if (= arg1 arg2) 1 0)]
    ; (println "=" val "\n")
    (write-result config val modes 2)))

; Opcode 9 adjusts the relative base by the value of its only parameter. The 
; relative base increases (or decreases, if the value is negative) by the value 
; of the parameter.
(defmethod execute-code 9 [{:keys [relative-base] :as config} _ modes]
  (let [arg1 (read-next-arg config modes 0)]
    ; (println "set relative base" relative-base arg1 modes "\n")
    (assoc config :relative-base (+ relative-base arg1))))

(defmethod execute-code 99 [config _ _]
  (assoc config :halted true))

(defmethod execute-code :default [{:keys [instructions pointer] :as config} _ _]
  (prn (str "Unknown code at " @pointer ": " (nth instructions @pointer)))
  config)

;
; runner
; 

(defn memory-to-vec [memory]
  (->> memory (sort-by key) (mapv second)))

(defn parse-opcode* [opcode]
  (->> opcode
       str
       (#(clojure.string/split % #""))
       (map parse-int)
       reverse))

(def parse-opcode (memoize parse-opcode*))

(defn next-opcode [config]
  (let [opcode (next-instruction config)]
    (assert (< 0 opcode))
    (if (or (< opcode 10) (= 99 opcode))
      [opcode]
      (parse-opcode opcode))))

(defn int-code-runner [config]
  (loop [config config]
    (let [[opcode _ & parameter-modes] (next-opcode config)
          result (execute-code config opcode parameter-modes)]
      (if (:halted result)
        (select-keys result [:output :instructions])
        (recur result)))))

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
