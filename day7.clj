(defn parse-line [line]
  (let [matcher (re-matcher #" (\w) " line)]
    {:from (-> matcher re-find last)
     :to   (-> matcher re-find last)}))

(defn read-file [file]
  (->> file
       slurp
       clojure.string/split-lines
       (map parse-line)))

(defn parse [file]
  (let [edges (read-file file)
        nodes (->> edges (map vals) flatten (into (sorted-set)))]
    (loop [result {}
           nodes nodes
           visited #{}]
      (if (empty? nodes)
        result
        (if (visited (first nodes))
          (recur result (rest nodes) visited)
          (let [node     (first nodes)
                children (->> edges (filter #(-> % :to (= node))) (map :from) (into (sorted-set)))
                next     (->> edges (filter #(-> % :from (= node))) (map :to))]
            (recur (assoc result node {:node node :children children :next next})
                   (disj nodes node)
                   (conj visited node))))))))

(def input (parse "input-7.txt"))
(def example (parse "input-7-example.txt"))

(defn get-starting-node [nodes]
  (->> nodes vals (filter #(-> % :children empty?)) first :node))

(defn node-available? [node visited]
  (clojure.set/subset? (:children node) visited))

(defn walk-three [nodes]
  ; (clojure.pprint/pprint nodes)
  (loop [queue (sorted-set (get-starting-node nodes))
         visited #{}
         path []]
    (if (empty? queue)
      path
      (let [node  (first queue)
            data  (get nodes node)
            queue (disj queue node)]
        (if (visited node)
          (recur queue visited path)
          (if (not (node-available? data visited))
            (recur (reduce conj queue (:children data)) visited path)
            (recur (reduce conj queue (:next data))
                   (conj visited node)
                   (conj path node))))))))

; PART 1
; (-> input walk-three clojure.string/join)
; "GLMVWXZDKOUCEJRHFAPITSBQNY"

;;
;; PART 2
;;

(def extra-seconds ["" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
                    "O" "P" "Q" "R" "S" "T" "U" "W" "V" "X" "Y" "Z"])

(defn available? [worker]
  (-> worker deref :time-left (= 0)))

(defn tick
  "decreases the time-left property of a worker"
  [worker]
  (reset! worker (update @worker :time-left #(max 0 (- % 1)))))

(defn assign [worker node]
  (let [duration (+ 60 (.indexOf extra-seconds (:node node)))]
    (reset! worker {:working-on node :time-left duration})))

(defn free-up [worker]
  (if (= 0 (:time-left @worker))
    (swap! worker assoc :working-on nil)))

(defn assign-chores [workers path nodes finished]
  (loop [workers workers
         path path]
    (if (or (empty? workers) (empty? path))
      path
      (let [next (->> path (map #(get nodes %)) (filter #(node-available? % finished)) first)]
        (if-not next
          path
          (do (assign (first workers) next)
              (recur (rest workers)
                     (remove #(= (:node next) %) path))))))))

(defn build [nodes path nr-of-workers]
  (let [workers (->> {:working-on nil :time-left 0} repeat (take nr-of-workers) (map atom))]
    (loop [queue       path
           time-passed 0
           finished     #{}]
      ; (println "\ntime-passed" time-passed)
      ; (prn "queue" queue)
      ; (prn "finished" finished)
      ; (clojure.pprint/pprint (->> workers (map deref) (map #(identity {:node (-> % :working-on :node) :time-left (:time-left %)}))))
      (let [available-workers (filter available? workers)]
        ; (prn "in-progress" in-progress)
        (if (and (empty? queue) (= nr-of-workers (count available-workers)))
          time-passed ; done!
          (let [path (assign-chores available-workers queue nodes finished)] ; assign chores
            (doseq [worker workers] (tick worker)) ; update time passed
            (recur path
                   (inc time-passed)
                   (reduce conj finished (->> workers
                                              (filter available?)
                                              (map #(-> % deref :working-on :node)))))))))))
