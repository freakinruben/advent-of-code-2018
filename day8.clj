(defn read-file [filename]
  (->> filename
       slurp
       (#(clojure.string/split % #" "))
       (map #(Integer/parseInt %))))

(defn parse [header]
  (let [meta-count  (second header)]
    (loop [child-count (first header)
           header      (drop 2 header)
           children    []]
      (if (or (empty? header) (= 0 child-count))
        {:node {:children children
                :metadata (take meta-count header)}
         :header (drop meta-count header)}
        (let [result (parse header)]
          (recur (dec child-count)
                 (:header result)
                 (conj children (:node result))))))))

(defn sum-metadata [node]
  (let [children-meta (map sum-metadata (:children node))]
    (->> children-meta
         (reduce +)
         (+ (reduce + (:metadata node))))))

(defn part1 []
  (-> "input-8.txt"
      read-file
      parse
      :node
      sum-metadata))

; 35852

(defn merge-children [node]
  (let [children (:children node)]
    (->> children
         (map merge-children)
         (filter #(not (empty? %)))
         (reduce conj children))))

(defn node-value [node]
  (if (nil? node)
    0
    (if (-> node :children empty?)
      (reduce + (:metadata node))
      (let [merged-children (merge-children node)]
        (->> node
             :metadata
             (map #(-> merged-children
                       (get (dec %))
                       node-value))
             (reduce +))))))

(defn part2 []
  (-> "input-8.txt"
      read-file
      parse
      :node
      node-value))

; (time (part2))
; "Elapsed time: 28.425139 msecs"
; 33422
