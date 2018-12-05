(def ids (->> "input-2.txt"
              slurp
              clojure.string/split-lines))

;
; Part 1
;

(defn find-duplicate-letters [id search-frequency]
  (some #(when (= search-frequency (val %))
           1)
        (frequencies id)))

(defn count-ids-with [ids search-frequency]
  (->> ids
       (map #(find-duplicate-letters % search-frequency))
       (filter some?)
       count))

(defn checksum [ids]
  (* (count-ids-with ids 2)
     (count-ids-with ids 3)))

(def sum (checksum ids)) ; 6000

;
; Part 2
;

(defn match-ids [id1 id2]
  (loop [matched []
         rest1 id1
         rest2 id2
         idx 0]
    (if (= (count rest1) 0) ; no more characters left to match
      (if (= 1 (- idx (count matched))) ; one char difference?
        [id1 id2] ; we have a match!
        nil)
      (if (> (- idx (count matched)) 1) ; more than 1 character difference
        nil
        (recur (if (= (first rest1) (first rest2))
                 (conj matched (first rest1))
                 matched)
               (rest rest1)
               (rest rest2)
               (inc idx))))))

(defn find-correct-id [cur-id all-ids]
  (->> all-ids
       (filter #(not= cur-id %))
       (map #(match-ids cur-id %))
       (filter some?)
       first))

(defn match-all-ids [all-ids]
  (->> all-ids
       (map #(find-correct-id % all-ids))
       (filter some?)
       first))

(def answer2 (match-all-ids ids))
; "Elapsed time: 93.515032 msecs"
; ["pbykrmjmizwhxlqnmasfgtycdv"
;  "pbykrmjmizwhxlqnwasfgtycdv"]
;   pbykrmjmizwhxlqnasfgtycdv

