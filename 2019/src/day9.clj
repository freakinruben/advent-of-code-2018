(ns day9
  (:require [day2 :refer [execute-code
                          parse-codes
                          parse-file
                          run
                          read-next-arg]]
            [day5]))

(def numbers (delay (parse-file "input9.txt")))

; Opcode 9 adjusts the relative base by the value of its only parameter. The 
; relative base increases (or decreases, if the value is negative) by the value 
; of the parameter.
(defmethod execute-code 9 [{:keys [relative-base] :as config} _ modes]
  (let [arg1 (read-next-arg config modes 0)]
    (assoc config :relative-base (+ relative-base arg1))))

(def example1 (parse-codes "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
(def example2 (parse-codes "1102,34915192,34915192,7,4,7,99,0"))
(def example3 (parse-codes "104,1125899906842624,99"))

(defn run-tests []
  (let [run1 (run example1)]
    (assert (= example1
               (->> run1 (take (count example1)) (zipmap (range))))))
  (prn "example 2 prints 16-digit nr?" (run example2))
  (prn "example 3 prints large nr in middle?" (run example3)))
