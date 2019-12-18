(ns day5
  (:require [day2 :refer [execute-code
                          parse-file
                          run
                          read-next-arg
                          write-result]]))

(def numbers (delay (parse-file "input5.txt")))

; jump back 1 position to make sure the next execute reads from the correct location
(defn jump-pointer [config position]
  (reset! (:pointer config) (dec position)))

; Opcode 3 takes a single integer as input and saves it to the position given by 
; its only parameter. For example, the instruction 3,50 would take an input 
; value and store it at address 50.
; 
; Input can be either a single value or a list. In case of a list, it will read
; the first item from the list and then remove it from the available inputs
(defmethod execute-code 3 [{:keys [input] :as config} _ modes]
  (assert (not= nil input))
  (if (seq input)
    ; (do (println "write input" (first input) "\n")
    (-> config
        (write-result (first input) modes 0)
        (assoc :input (rest input))) ; remove read item from input
    ; (do (println "write input" input "\n")
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

; Your ship computer already understands parameter mode 0, position mode, 
; which causes the parameter to be interpreted as a position - if the parameter 
; is 50, its value is the value stored at address 50 in memory. Until now, all 
; parameters have been in position mode.
; 
; Your ship computer will also need to handle parameters in mode 1, immediate 
; mode. In immediate mode, a parameter is interpreted as a value - if the 
; parameter is 50, its value is simply 50.

(def answer1 (-> @numbers (run 1) :instructions (get 0) delay))
(def answer2 (-> @numbers (run 5) :instructions (get 0) delay))
