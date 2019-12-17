(ns day1)

; Fuel required to launch a given module is based on its mass. Specifically,
; to find the fuel required for a module,
; take its mass,
; divide by three,
; round down,
; and subtract 2.

(def numbers
  (->> "input1.txt"
       clojure.java.io/resource
       slurp
       clojure.string/split-lines
       (map #(Integer/parseInt %))
       delay))

(defn calc-module-fuel [module-mass]
  (-> module-mass
      (/ 3)
      float
      Math/floor
      (- 2)))


(defn total-fuel [modules]
  (->> modules
       (map calc-module-fuel)
       (reduce +)))

(def answer1 (total-fuel @numbers))

(defn calc-fuel-fuel [fuel-mass]
  (let [fuel-fuel (calc-module-fuel fuel-mass)]
    (if (<= fuel-fuel 0)
      0
      (+ fuel-fuel
         (calc-fuel-fuel fuel-fuel)))))

(defn improved-calc-module-fuel [module-mass]
  (let [fuel (calc-module-fuel module-mass)]
    (+ fuel
       (calc-fuel-fuel fuel))))

(defn improved-total-fuel [modules]
  (->> modules
       (map improved-calc-module-fuel)
       (reduce +)))

(def answer2 (improved-total-fuel @numbers))
