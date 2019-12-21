(ns day5
  (:require intcode))

(def numbers (delay (intcode/parse-file "input5.txt")))

(def answer1 (-> @numbers (intcode/run 1) :instructions (get 0) delay))
(def answer2 (-> @numbers (intcode/run 5) :instructions (get 0) delay))
