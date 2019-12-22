(ns day21
  (:require intcode
            pathfinder))

(def instructions (delay (intcode/parse-file "input21.txt")))

(defn run-springbot [instructions input]
  (let [output (-> instructions (intcode/run input) :output time)]
    (->> output (mapv char) (clojure.string/join "") println)))

; The springdroid will move forward automatically, constantly thinking about 
; whether to jump. The springscript program defines the logic for this decision.
;
; Programs only use Boolean values, not numbers or strings. Two registers are 
; available: 
; - T, the temporary value register, 
; - J, the jump register. 
;
; If the jump register is true at the end of the springscript program, the 
; springdroid will try to jump. Both of these registers start with the value false
;
; - it can only remember at most 15 springscript instructions.
; - have a sensor that can detect whether there is ground at various distances 
;   in the direction it is facing
; - these values are provided in read-only registers
; - If there is ground at the given distance, the register will be true; 
; - if there is a hole, the register will be false.
; - can detect ground at four distances: 
;   - 1 tile away (A), 
;   - 2 tiles away (B), 
;   - 3 tiles away (C), 
;   - 4 tiles away (D)
;
; There are only three instructions available in springscript:
; 
; - AND X Y sets Y to true if both X and Y are true; otherwise, it sets Y to false.
; - OR X Y sets Y to true if at least one of X or Y is true; otherwise, it sets Y to false.
; - NOT X Y sets Y to true if X is false; otherwise, it sets Y to false.
;
; In all three instructions, the second argument (Y) needs to be a writable 
; register (either T or J). The first argument (X) can be any register 
; (including A, B, C, or D).

(defn str-to-ascii [input-str]
  (conj (mapv int input-str) 10))

(defn ascii-input [input]
  (->> (conj input "WALK")
       (mapv str-to-ascii)
       (apply concat)))

(defn figure-out-input [instructions]
  (let [input ["NOT D J"]]
    ; (prn input)
    ; (prn (ascii-input input))
    ; (println (mapv char (ascii-input input)))
    (run-springbot instructions (ascii-input input))))

(def answer1 (delay (figure-out-input @instructions)))
