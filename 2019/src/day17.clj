(ns day17
  (:require [day2 :refer [parse-file
                          run]]
            [day5]
            [day9]))

(defonce numbers (delay (parse-file "input17.txt")))

(defonce camera-output (-> @numbers run :output time delay))

(defn draw-camera-output [output]
  (->> output
       (map char)
       clojure.string/join
       print-str))

(defn draw-parsed-output [output]
  (->> output
       (map :symbol)
       clojure.string/join
       print-str))

(derive ::newline      ::symbol)
(derive ::open-space   ::symbol)

(derive ::robot        ::symbol)
(derive ::left         ::robot)
(derive ::right        ::robot)
(derive ::up           ::robot)
(derive ::down         ::robot)

(derive ::scafold      ::symbol)
(derive ::intersection ::scafold)
(derive ::left         ::scafold)
(derive ::right        ::scafold)
(derive ::up           ::scafold)
(derive ::down         ::scafold)

(defn translate-symbol [symbol-code]
  (let [symbol (char symbol-code)]
    (condp = symbol
      \#       {:symbol symbol :name ::scafold}
      \.       {:symbol symbol :name ::open-space}
      \newline {:symbol symbol :name ::newline}
      \>       {:symbol symbol :name ::right}
      \<       {:symbol symbol :name ::left}
      \^       {:symbol symbol :name ::up}
      \v       {:symbol symbol :name ::down})))

(defn set-location [width idx pixel]
  (assoc pixel
         :location idx
         :x (mod idx width)
         :y (-> idx (/ width) Math/floor int)))

(defn get-width [output]
  (some #(when (-> % :name (= ::newline))
           (-> % :location inc))
        output))
; (def get-width (memoize get-width*))

(defn get-surroundings [width output pixel]
  (let [left  (-> pixel :location (- 1))
        right (-> pixel :location (+ 1))
        up    (-> pixel :location (- width))
        down  (-> pixel :location (+ width))]
    (->> [left right up down]
         (map #(nth output % nil))
         (zipmap [::left ::right ::up ::down]))))

(defn is-scafold? [pixel]
  (-> pixel :name (isa? ::scafold)))

(defn is-intersection? [width output pixel]
  (let [surroundings (get-surroundings width output pixel)]
    (->> surroundings
         vals
         (every? is-scafold?))))

(defn parse-camera-output [camera-output]
  (let [img-width (inc (.indexOf camera-output (int \newline)))
        output (->> camera-output
                    (map translate-symbol)
                    (map-indexed (partial set-location img-width)))]
    (loop [to-check output
           new-output []]
      (if (seq to-check)
        (let [pixel (first to-check)]
          (if (is-intersection? img-width output pixel)
            (recur (rest to-check)
                   (conj new-output (assoc pixel :name ::intersection :symbol \0)))
            (recur (rest to-check)
                   (conj new-output pixel))))
        new-output))))
; 
; part 1
;

(def answer1 (->> @camera-output
                  parse-camera-output
                  (filter #(-> % :name (= ::intersection)))
                  (map #(* (:x %) (:y %)))
                  (reduce +)
                  time
                  delay))

;
; PART 2
; 1. PATH FINDING
;

(defn wake-up-robot [numbers]
  (-> numbers (assoc 0 2)))

(defn find-robot [camera-output]
  (->> camera-output
       (filter #(-> % :name (isa? ::robot)))
       first))

(defn find-next-scafold
  "When given pixels in different directions, it will return the direction of the
   next pixel that is a scafold"
  [all-pixels keys-to-look-at]
  (->> (select-keys all-pixels keys-to-look-at)
       (some (fn [[dir pxl]]
               (when (is-scafold? pxl)
                 dir)))))

(defn turn-robot
  "turn robot to corner. This fn assumes the robot is at a corner of the path"
  [camera-output robot-pixel]
  (let [width (get-width camera-output)
        surroundings (get-surroundings width camera-output robot-pixel)
        new-direction (condp = (:name robot-pixel)
                        ::left  (find-next-scafold surroundings [::down ::up])
                        ::down  (find-next-scafold surroundings [::right ::left])
                        ::up    (find-next-scafold surroundings [::right ::left])
                        ::right (find-next-scafold surroundings [::down ::up]))]
    (when new-direction ; will be nil at the end of the path
      (assoc robot-pixel :name new-direction))))

(defn walk-robot
  "walks the robot down a straight path. Returns a list with all the walked pixels"
  [camera-output robot-pixel]
  (let [walk-offset (condp = (:name robot-pixel)
                      ::left -1
                      ::right 1
                      ::down (get-width camera-output)
                      ::up   (* -1 (get-width camera-output)))]
    (loop [walked []
           cur-pixel robot-pixel]
      (let [next-location (-> cur-pixel :location (+ walk-offset))
            next-pixel (nth camera-output next-location nil)]
        (if (some-> next-pixel is-scafold?)
          (recur (conj walked next-pixel)
                 next-pixel)
          walked)))))

(defn relative-turn
  "translate absolute direction changes (from ::left to ::down) into either R or L"
  [start-pixel end-pixel]
  (condp = [(:name start-pixel) (:name end-pixel)]
    [::up    ::left]  "L"
    [::up    ::right] "R"
    [::down  ::left]  "R"
    [::down  ::right] "L"
    [::left  ::down]  "L"
    [::left  ::up]    "R"
    [::right ::down]  "R"
    [::right ::up]    "L"))

(defn find-path [camera-output]
  (loop [path []
         robot-pixel (find-robot camera-output)]
    (if-let [turned (turn-robot camera-output robot-pixel)]
      (let [walked (walk-robot camera-output turned)]
        (recur (-> path
                   (conj (relative-turn robot-pixel turned))   ; add L or R for the turn direction
                   (conj (count walked)))                      ; add nr of steps walked
               (-> walked last (assoc :name (:name turned))))) ; put robot on last walked pixel
      path)))

;
; 2. FIND MAIN FUNCTIONS
;

(defn find-first-duplicate
  "Looks if the needle-list appears somewhere in the collection"
  [coll needle]
  (loop [coll coll]
    (if (seq coll)
      (if (= needle (take (count needle) coll))
        true
        (recur (rest coll)))
      false)))

; movement functions may each contain at most 20 characters
; = 7 direction, 7 length, 6 commas; therefor max 14 items from collection
; but it only seems to work when using 12..
(def max-movement-functions 12)

(defn find-max-duplicates
  "Greedy find how many items it can take from coll that repeat later in collection"
  [coll]
  (loop [items-to-search 2
         pattern-found []]
    (if (or (= items-to-search (count coll))
            (= items-to-search max-movement-functions))
      pattern-found
      (let [needle (take items-to-search coll)]
        (if (find-first-duplicate (drop items-to-search coll) needle)
          (recur (+ 2 items-to-search)
                 needle)
          pattern-found)))))

(defn remove-duplicates
  "removes the given duplicate from the collection"
  [coll duplicate]
  (let [len (count duplicate)]
    (loop [new-coll []
           position 0]
      (if (< position (count coll))
        (let [to-check (->> coll (drop position) (take len))]
          (if (= to-check duplicate)
            (recur new-coll
                   (+ position len)) ; found it, skip these items
            (recur (conj new-coll (first to-check))
                   (inc position)))) ; not found, lets check next item
        new-coll))))

(defn find-patterns
  "finds recurring patterns in a given collection (should be 3 patterns)"
  [coll]
  (loop [coll coll
         duplicates []]
    (if (seq coll)
      (let [next-duplicate (find-max-duplicates coll)]
        (if (seq next-duplicate)
          (recur (remove-duplicates coll next-duplicate)
                 (conj duplicates next-duplicate))
          (conj duplicates coll)))
      duplicates)))

;
; 3. MAKE INPUTS
;

(defn find-main-input
  "Returns a list with A/B/C based on which pattern is found"
  [path patterns]
  (let [[a b c] patterns]
    (loop [path path
           main-input []]
      (if (seq path)
        (cond
          (= a (take (count a) path)) (recur (drop (count a) path)
                                             (conj main-input "A"))
          (= b (take (count b) path)) (recur (drop (count b) path)
                                             (conj main-input "B"))
          (= c (take (count c) path)) (recur (drop (count c) path)
                                             (conj main-input "C"))
          :else (throw (Exception. (str "Unknown pattern: "
                                        (clojure.string/join " " path)))))
        main-input))))

(def answer2
  (delay
   (let [image      (time (parse-camera-output @camera-output))
         path       (time (find-path image))
         patterns   (time (find-patterns path))
         main-input (time (find-main-input path patterns))
         inputs     (->> patterns                           ; MAIN A B C
                         (cons main-input)                  ; Add main pattern => MAIN A B C
                         vec
                         (#(conj % ["n"]))                  ; No continuous video-feed => MAIN A B C n
                         (map #(clojure.string/join "," %)) ; add commas
                         (map #(map int %))                 ; Convert instructions to ASCII
                         (map vec)                          ; convert to vectors before using conj (otherwise it appends to the beginning)
                         (map #(conj % 10))                 ; Add newlines
                         (apply concat)                     ; merge into one list
                         time)]
     (-> @numbers
         wake-up-robot
         (run inputs)
         :output
         last
         time))))
