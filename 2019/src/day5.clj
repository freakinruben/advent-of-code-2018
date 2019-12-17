(ns day5
  (:require [day2 :refer [execute-code
                          parse-file
                          parse-int
                          read-memory
                          run
                          read-next-arg
                          write-result]]))

(def numbers (delay (parse-file "input5.txt")))

(defn parse-opcode [opcode]
  (->> opcode
       str
       (#(clojure.string/split % #""))
       (map parse-int)
       reverse))

; jump back 1 position to make sure the next execute reads from the correct location
(defn jump-pointer [config position]
  (reset! (:pointer config) (dec position)))

; Opcode 3 takes a single integer as input and saves it to the position given by 
; its only parameter. For example, the instruction 3,50 would take an input 
; value and store it at address 50.
(defmethod execute-code 3 [{:keys [input] :as config} _ _]
  (write-result config input))

; Opcode 4 outputs the value of its only parameter. For example, the instruction 
; 4,50 would output the value at address 50.
(defmethod execute-code 4 [config _ modes]
  (update-in config [:output] #(conj % (read-next-arg config modes 0))))

; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the 
; instruction pointer to the value from the second parameter. Otherwise, it does 
; nothing.
(defmethod execute-code 5 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)]
    (when (> arg1 0)
      (jump-pointer config arg2))
    config))

; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the 
; instruction pointer to the value from the second parameter. Otherwise, it does 
; nothing.
(defmethod execute-code 6 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)]
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
    (write-result config val)))

; Opcode 8 is equals: if the first parameter is equal to the second parameter, 
; it stores 1 in the position given by the third parameter. Otherwise, it 
; stores 0.
(defmethod execute-code 8 [config _ modes]
  (let [arg1 (read-next-arg config modes 0)
        arg2 (read-next-arg config modes 1)
        val  (if (= arg1 arg2) 1 0)]
    (write-result config val)))

(defmethod execute-code :default [{:keys [pointer] :as config} _ _]
  (let [[opcode _ & param-modes :as all] (parse-opcode (read-memory config @pointer))]
    (try
      (assert (= param-modes
                 (->> param-modes
                      (filter #(< % 3))
                      seq)))
      (assert (< 0 opcode 10))
      (execute-code config opcode param-modes)

      (catch Throwable e
        (println "Error:" (ex-data e))
        (println (str "\tUnknown code at " @pointer ": " (read-memory config @pointer)))
        (println (str "\topcode: " opcode "; params: " param-modes "; all: " all))
        (throw e)))))

; Your ship computer already understands parameter mode 0, position mode, 
; which causes the parameter to be interpreted as a position - if the parameter 
; is 50, its value is the value stored at address 50 in memory. Until now, all 
; parameters have been in position mode.
; 
; Your ship computer will also need to handle parameters in mode 1, immediate 
; mode. In immediate mode, a parameter is interpreted as a value - if the 
; parameter is 50, its value is simply 50.

(def answer1 (-> @numbers (run 1) first delay))
(def answer2 (-> @numbers (run 5) first delay))
