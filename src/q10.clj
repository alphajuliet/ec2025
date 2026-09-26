(ns q10
  (:require [clojure.core.matrix :as m]
            [clojure.set :as set]
            [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (let [m (->> f
               slurp
               str/split-lines
               (map #(str/split % #"")))
        size (m/shape m)
        sheep (util/mfind-all m "S")
        dragon (util/mfind-all m "D")
        hideouts (util/mfind-all m "#")]
    {:size size :sheep sheep :dragon dragon :hideouts hideouts :eaten 0}))

(defn dragon-moves
  "Return all the legal dragon moves from a given location in a field of a given size."
  ;; dragon-moves : Vector Int -> Vector Int -> Coll (Vector Int)
  [[rmax cmax] [r c]]
  (let [deltas (for [dr (range -2 3)
                     dc (range -2 3)
                     :when (= 3 (+ (abs dr) (abs dc)))
                     :when (and (<= 0 (+ r dr) (dec rmax))
                                (<= 0 (+ c dc) (dec cmax)))]
                 [(+ r dr) (+ c dc)])]
    deltas))

(defn reachable-positions
  "Return the set of all positions reachable from `start` by making at most
  `depth` successive dragon moves on a field of the given `size`.
  `size` is a [rows cols] vector (as returned by `read-data`) and `start` is
  an [r c] position.  The result includes `start` itself, i.e. the zero-move
  case."
  ;; reachable-positions : Vector Int -> Vector Int -> Int -> Set Int
  ([size start depth]
   (reachable-positions size [start] depth #{start}))
  ([size frontier depth seen]
   (if (or (zero? depth) (empty? frontier))
     seen
     (let [moves (->> frontier
                      (mapcat #(dragon-moves size %))
                      (remove seen)
                      distinct
                      vec)]
       (reachable-positions size moves (dec depth) (into seen moves))))))

(defn expand-dragon
  "Move the dragon one step: its possible positions become every square reachable
   in one move from any of its current positions, replacing the old ones."
  [{:keys [size dragon] :as state}]
  (assoc state :dragon (->> dragon
                            (mapcat (partial dragon-moves size))
                            distinct
                            vec)))

(defn move-sheep
  "Move all the sheep down one square and remove those that roll off the bottom"
  [{:keys [:size] :as state}]
  (let [rmax (first size)]
    (-> state
        (update :sheep (partial mapv #(update % 0 inc)))
        (update :sheep (partial filter #(<= (first %) (dec rmax)))))))

(defn eat-sheep
  "Remove the sheep standing on a square the dragon can reach that is not a hideout,
   i.e. sheep <- sheep & !(dragon & !hideouts), and add them to the eaten count."
  [{:keys [sheep dragon hideouts] :as state}]
  (let [exposed (set/difference (set dragon) (set hideouts))
        {eaten true safe false} (group-by (comp boolean exposed) sheep)]
    (-> state
        (assoc :sheep (vec safe))
        (update :eaten (fnil + 0) (count eaten)))))

(defn run-steps
  "Run n turns of alternately extending the dragon's reach and moving the sheep."
  [{:keys [hideouts] :as state} turns]
  (reduce 
    (fn [s _]
      (-> s
          expand-dragon
          eat-sheep
          move-sheep
          eat-sheep))
    state
    (range turns)))

(defn part1
  "Solution for part 1"
  [fname depth]
  (let [{:keys [size sheep dragon]} (read-data fname)
        extent (reachable-positions size (first dragon) depth)]
    (count (set/intersection extent (set sheep)))))

(defn part2
  "Solution for part 2"
  [fname rounds]
  (-> fname
      read-data
      (run-steps rounds)
      :eaten))

(comment
  (def testf1 "data/q10_p1_test.txt")
  (def inputf1 "data/q10_p1.txt")
  (def testf2 "data/q10_p2_test.txt")
  (def inputf2 "data/q10_p2.txt")

  (part1 testf1 3)
  (part1 inputf1 4)

  (part2 testf2 3)
  (part2 inputf2 20))

;; The End
