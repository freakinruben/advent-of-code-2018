(ns day9
  (:require intcode))

(def numbers (delay (intcode/parse-file "input9.txt")))

(def example1 (intcode/parse-codes "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
(def example2 (intcode/parse-codes "1102,34915192,34915192,7,4,7,99,0"))
(def example3 (intcode/parse-codes "104,1125899906842624,99"))

(defn run-tests []
  (let [run1 (intcode/run example1)]
    (prn run1)
    (assert (= (intcode/memory-to-vec example1)
               (:output run1))))
  (prn "example 2 prints 16-digit nr?" (intcode/run example2))
  (prn "example 3 prints large nr in middle?" (intcode/run example3)))

(def answer1 (-> @numbers (intcode/run 1) :output first delay))
(def answer2 (-> @numbers (intcode/run 2) :output delay))
