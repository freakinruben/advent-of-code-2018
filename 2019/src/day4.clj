(ns day4)

; However, they do remember a few key facts about the password:
;
;   - It is a six-digit number.
;   - The value is within the range given in your puzzle input.
;   - Two adjacent digits are the same (like 22 in 122345).
;   - Going from left to right, the digits never decrease; they only ever 
;     increase or stay the same (like 111123 or 135679).
;
; Other than the range rule, the following are true:
;
;   - 111111 meets these criteria (double 11, never decreases).
;   - 223450 does not meet these criteria (decreasing pair of digits 50).
;   - 123789 does not meet these criteria (no double).
;
; How many different passwords within the range given in your puzzle input meet 
; these criteria?

(def input "307237-769058")

(defn parse-int [i] (Integer/parseInt i))

(defn parse-input [in]
  (map parse-int (clojure.string/split in #"-")))

(defn split-nr [nr]
  (map parse-int (clojure.string/split (str nr) #"")))

(defn digits-increase? [nr-list]
  (loop [nr-list nr-list]
    (if (> (count nr-list) 1)
      (let [[a b] nr-list]
        (if (<= a b)
          (recur (rest nr-list))
          false))
      true)))

(defn same-adjecent-digits? [nr-list]
  (loop [nr-list nr-list]
    (if (> (count nr-list) 1)
      (let [[a b] nr-list]
        (if (= a b)
          true
          (recur (rest nr-list))))
      false)))

(defn find-password-combis [nrs]
  (->> nrs
       (map split-nr)
       (filter digits-increase?)
       (filter same-adjecent-digits?)
       doall
       time))

(defn make-password-combinations [[start end]]
  (let [all (range start end)]
    (->> all
         (partition-all 10000)
         (pmap find-password-combis)
         (map count)
         (reduce +)
         time)))

(def answer1 (-> input parse-input make-password-combinations delay))
