(ns day5
  (:require [day2 :refer [execute-code
                          parse
                          parse-int
                          run
                          read-next-arg
                          write-result]]))

(def numbers (delay (parse "input5.txt")))

(defn ask-input* []
  (prn "Input code:")
  (flush)
  (parse-int (read-line)))

(def ask-input (memoize ask-input*))

(defn parse-opcode [opcode]
  (->> opcode
       str
       (#(clojure.string/split % #""))
       (map parse-int)
       reverse))

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

(defmethod execute-code :default [_ instructions pointer _]
  (let [[opcode _ & param-modes :as all] (parse-opcode (nth instructions @pointer))]
    (try
      (assert (= param-modes
                 (->> param-modes
                      (filter #(< % 2))
                      seq)))
      (assert (< 0 opcode 5))
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

(def answer1 (delay (run @numbers)))
