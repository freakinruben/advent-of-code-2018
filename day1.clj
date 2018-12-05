(def numbers (->> "input-1.txt"
                  slurp
                  clojure.string/split-lines
                  (map #(Integer/parseInt %))))

(def answer1 (apply + numbers)) ; 522

(defn find-duplicate-frequency [all-changes]
  (loop [previous 0
         knowns #{previous}
         changes all-changes]
    (let [changes (if (empty? changes) all-changes changes) ; reset changes
          next (+ previous (first changes))]
      (if (contains? knowns next)
        next
        (recur next
               (conj knowns next)
               (rest changes))))))

(def answer2 (find-duplicate-frequency numbers)) ; 73364
