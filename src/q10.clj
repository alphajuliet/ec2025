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
        dragon (util/mfind-all m "D")]
    {:size size :sheep sheep :dragon dragon}))

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

(defn part1
  "Solution for part 1"
  [fname depth]
  (let [{:keys [size sheep dragon]} (read-data fname)
        extent (reachable-positions size (first dragon) depth)]
    (count (set/intersection extent (set sheep)))))

(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q10_p1_test.txt")
  (def inputf1 "data/q10_p1.txt")
  (def testf2 "data/q10_p2_test.txt")
  (def inputf2 "data/q10_p2.txt")

  (part1 testf1 3)
  (part1 inputf1 4)

  (part2 testf2)
  (part2 inputf2))
;; The End
