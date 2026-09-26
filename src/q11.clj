(ns q11
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       (str/split-lines)
       (mapv Integer/parseInt)))

(defn donate-if
  "If (a pred b) then donate from a to b, otherwise leave unchanged."
  [pred [a b]]
  (if (pred a b)
    [(dec a) (inc b)]
    [a b]))

(defn phase1
  "One round of phase 1: sweep left to right over adjacent column pairs,
  moving a duck rightwards wherever the left column has more ducks.
  Each move is visible to the next comparison."
  [cols]
  (reduce (fn [v i]
            (let [[a b] (donate-if > [(v i) (v (inc i))])]
              (assoc v i a (inc i) b)))
          cols
          (range (dec (count cols)))))

(defn phase2
  "One round of phase 2: sweep left to right over adjacent column pairs,
  moving a duck leftwards wherever the right column has more ducks.
  Each move is visible to the next comparison."
  [cols]
  (reduce (fn [v i]
            (let [[b a] (donate-if > [(v (inc i)) (v i)])]
              (assoc v i a (inc i) b)))
          cols
          (range (dec (count cols)))))

(defn iterate-n
  "Iterate n times on (f x)"
  [n f x]
  (last (take n (iterate f x))))

(defn fixed-point
  "Iterate a function until a fixed point is reached. Return [fixed-point n],
  where n is the number of iterations that changed the value."
  [f x]
  (->> (iterate f x)
       (partition 2 1)
       (keep-indexed (fn [i [a b]] (when (= a b) [a i])))
       first))
 
(defn run-phases
  "Run n total rounds across the two phases."
  [cols n]
  (let [[cols' rounds] (fixed-point phase1 cols)]
    (->> cols'
         (iterate-n (inc (- n rounds)) phase2))))

(defn part1
  "Solution for part 1"
  [fname]
  (-> fname
      read-data
      (run-phases 10)
      util/score-posn))

(defn part2
  "Solution for part 2"
  [fname]
  (let [cols (read-data fname)
        [cols' r1] (fixed-point phase1 cols)
        [_ r2] (fixed-point phase2 cols')]
    (+ r1 r2)))

(comment
  (def testf1 "data/q11_p1_test.txt")
  (def inputf1 "data/q11_p1.txt")
  (def testf2 "data/q11_p2_test.txt")
  (def inputf2 "data/q11_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))

;; The End
