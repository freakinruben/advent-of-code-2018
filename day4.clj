(def begin-shift #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+)")
(def asleep #"(\d+):(\d+)\] falls asleep")
(def wakes-up #"(\d+):(\d+)\] wakes up")

(defn parse-begin-shift [row]
  (let [matches (re-find begin-shift row)]
    {;:year (Integer/parseInt (nth matches 1))
     ;:month (Integer/parseInt (nth matches 2))
     ;:day (Integer/parseInt (nth matches 3))
     :hour   (Integer/parseInt (nth matches 4))
     :minute (Integer/parseInt (nth matches 5))
     :guard  (Integer/parseInt (nth matches 6))}))

(defn parse-asleep [row]
  (let [matches (re-find asleep row)]
    {:hour   (Integer/parseInt (nth matches 1))
     :minute (Integer/parseInt (nth matches 2))}))

(defn parse-wakes-up [row]
  (let [matches (re-find wakes-up row)]
    {:hour   (Integer/parseInt (nth matches 1))
     :minute (Integer/parseInt (nth matches 2))}))

(defn parse-sleep [asleep wakes-up]
  {:from (parse-asleep asleep)
   :to   (parse-wakes-up wakes-up)})

(defn parse-sleep-rows [rows]
  (loop [rows   rows
         result []]
    (if (or (empty? rows)
            (not (re-find asleep (first rows))))
      result
      (let [sleep (parse-sleep (first rows) (second rows))]
        (recur (drop 2 rows)
               (conj result sleep))))))

(defn parse-rows [log]
  (loop [rows log
         shifts []]
    (if (empty? rows)
      shifts
      (let [shift (parse-begin-shift (first rows))
            asleep (parse-sleep-rows (rest rows))
            read (+ 1 (* 2 (count asleep)))]
        (recur (drop read rows)
               (conj shifts (assoc shift :asleep asleep)))))))

(defn parse [file]
  (->> file
       slurp
       clojure.string/split-lines
       sort
       parse-rows))

(def input (parse "input-4.txt"))
(def example (parse "input-4-example.txt"))

;
; PART 1
;

(defn group-sleep-records [shifts]
  (reduce (fn [acc shift]
            (apply conj acc (:asleep shift)))
          [] shifts))

(defn calculate-sleep [[guard records]]
  (let [asleep (->> records
                    group-sleep-records
                    (map #(- (-> % :to :minute) (-> % :from :minute)))
                    (apply +))]
    [guard asleep]))

(defn was-asleep? [minute record]
  (and (>= minute (-> record :from :minute))
       (<  minute (-> record :to :minute))))

(defn sleep-per-minute [shifts]
  (let [shifts (group-sleep-records shifts)]
    (loop [minutes (range 0 59)
           result []]
      (if (empty? minutes)
        result
        (let [minute (first minutes)
              asleep (->> shifts
                          (map #(was-asleep? minute %))
                          (filter true?)
                          count)]
          (recur (rest minutes)
                 (conj result [minute asleep])))))))

(defn get-most-sleepy-minute [shifts])


(defn part1 [input]
  (let [shifts-per-guard (->> input
                              (group-by :guard))
        most-sleepy-guard (->> shifts-per-guard
                               (map calculate-sleep)
                               (sort-by second >)
                               first)
        most-sleepy-time (->> most-sleepy-guard
                              first
                              (get shifts-per-guard)
                              sleep-per-minute
                              (sort-by second >)
                              first)]
    (* (first most-sleepy-guard) (first most-sleepy-time))))
