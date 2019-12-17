(ns day5
  (:require [day2 :refer [ask-input
                          execute-code
                          parse-file
                          parse-int
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

(defn jump-pointer [pointer position]
  (reset! pointer (dec position))) ; jump back 1 position to make sure the next execute reads from the correct location

; Opcode 3 takes a single integer as input and saves it to the position given by 
; its only parameter. For example, the instruction 3,50 would take an input 
; value and store it at address 50.
(defmethod execute-code 3 [_ instructions pointer _]
  (let [input (ask-input)]
    (write-result instructions pointer input)))

; Opcode 4 outputs the value of its only parameter. For example, the instruction 
; 4,50 would output the value at address 50.
(defmethod execute-code 4 [_ instructions pointer modes]
  (prn "diagnostic-code" (read-next-arg instructions pointer (nth modes 0 0)))
  instructions)

; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the 
; instruction pointer to the value from the second parameter. Otherwise, it does 
; nothing.
(defmethod execute-code 5 [_ instructions pointer modes]
  (let [arg1 (read-next-arg instructions pointer (nth modes 0 0))
        arg2 (read-next-arg instructions pointer (nth modes 1 0))]
    (when (> arg1 0)
      (jump-pointer pointer arg2))
    instructions))

; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the 
; instruction pointer to the value from the second parameter. Otherwise, it does 
; nothing.
(defmethod execute-code 6 [_ instructions pointer modes]
  (let [arg1 (read-next-arg instructions pointer (nth modes 0 0))
        arg2 (read-next-arg instructions pointer (nth modes 1 0))]
    (when (= arg1 0)
      (jump-pointer pointer arg2))
    instructions))

; Opcode 7 is less than: if the first parameter is less than the second 
; parameter, it stores 1 in the position given by the third parameter. 
; Otherwise, it stores 0.
(defmethod execute-code 7 [_ instructions pointer modes]
  (let [arg1 (read-next-arg instructions pointer (nth modes 0 0))
        arg2 (read-next-arg instructions pointer (nth modes 1 0))
        val  (if (< arg1 arg2) 1 0)]
    (write-result instructions pointer val)))

; Opcode 8 is equals: if the first parameter is equal to the second parameter, 
; it stores 1 in the position given by the third parameter. Otherwise, it 
; stores 0.
(defmethod execute-code 8 [_ instructions pointer modes]
  (let [arg1 (read-next-arg instructions pointer (nth modes 0 0))
        arg2 (read-next-arg instructions pointer (nth modes 1 0))
        val  (if (= arg1 arg2) 1 0)]
    (write-result instructions pointer val)))

(defmethod execute-code :default [_ instructions pointer _]
  (let [[opcode _ & param-modes :as all] (parse-opcode (nth instructions @pointer))]
    (try
      (assert (= param-modes
                 (->> param-modes
                      (filter #(< % 2))
                      seq)))
      (assert (< 0 opcode 9))
      (execute-code opcode instructions pointer param-modes)

      (catch Throwable e
        (println "Error:" (ex-data e))
        (println (str "\tUnknown code at " @pointer ": " (nth instructions @pointer)))
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

(def answer1 (delay (run @numbers 1)))
(def answer2 (delay (run @numbers 5)))
