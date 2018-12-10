(defrecord Chain [value prev next])

(defn linked-list [value]
  (let [self (atom (->Chain value nil nil))]
    (swap! self assoc :prev self)
    (swap! self assoc :next self)
    self))

(defn move-forward [chain steps]
  (loop [steps steps
         pointer chain]
    (if (= 0 steps)
      pointer
      (recur (dec steps) (:next @pointer)))))

(defn move-backwards [chain steps]
  (loop [steps steps
         pointer chain]
    (if (= 0 steps)
      pointer
      (recur (dec steps) (:prev @pointer)))))

(defn add! [current new-value]
  (let [new (atom (->Chain new-value current (:next @current)))]
    (swap! (:next @current) assoc :prev new)
    (swap! current assoc :next new)
    new))

(defn remove! [current]
  (let [next (:next @current)
        prev (:prev @current)]
    (swap! prev assoc :next next)
    (swap! next assoc :prev prev)
    (reset! current {})
    next))

(def r-outcome #"(\d+) players; last marble is worth (\d+) points(: high score is (\d+))?")

(defn parse [outcome]
  (let [parsed (re-find r-outcome outcome)]
    {:player-count (some-> parsed (nth 1) Integer/parseInt)
     :last-marble  (some-> parsed (nth 2) Integer/parseInt)
     :high-score   (some-> parsed (nth 4) Integer/parseInt)}))

(def input (parse "429 players; last marble is worth 70901 points"))
(def input2 (parse "429 players; last marble is worth 7090100 points"))
(def example1 (parse "10 players; last marble is worth 1618 points: high score is 8317"))
(def example2 (parse "13 players; last marble is worth 7999 points: high score is 146373"))
(def example3 (parse "17 players; last marble is worth 1104 points: high score is 2764"))
(def example4 (parse "21 players; last marble is worth 6111 points: high score is 54718"))
(def example5 (parse "30 players; last marble is worth 5807 points: high score is 37305"))

(defn play-game [{:keys [player-count last-marble] :as setup}]
  (prn "setup" setup)
  (let [scores (->> 0 repeat (take player-count) (map atom) doall)]
    (loop [nxt-marble 1
           cur-marble (linked-list 0)]
      (if (= nxt-marble last-marble)
        (->> scores (map deref) (reduce max))
        (if (= (mod nxt-marble 23) 0)
          (let [player    (mod nxt-marble player-count)
                bonus     (move-backwards cur-marble 7)
                score     (nth scores player)]
            (swap! score #(+ % nxt-marble (:value @bonus)))
            (recur (inc nxt-marble)
                   (remove! bonus)))
          (recur (inc nxt-marble)
                 (-> cur-marble (move-forward 1) (add! nxt-marble))))))))

; (time (play-game input))
; "Elapsed time: 937534.910285 msecs"
; 399645
; "Elapsed time: 73.117999 msecs"
; 399645

; (time (play-game input2))
; "Elapsed time: 7339.036319 msecs"
; 3352507536
