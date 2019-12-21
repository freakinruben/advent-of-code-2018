(ns day2
  (:require intcode))

(def numbers (delay (intcode/parse-file "input2.txt")))
(def answer1 (-> @numbers (intcode/run 12 2) :instructions (get 0) delay))

(defn find-verb [input desired-result max-loop noun]
  (loop [verbs (range 0 max-loop)]
    (when-let [verb (first verbs)]
      (let [result (-> input (intcode/run noun verb) :instructions (get 0))]
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
