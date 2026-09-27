(ns q12
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [util :as util]
            [search :as sch]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))
       (util/mapmap Integer/parseInt)))

(defn neighbours
  "Return the neighbours of the given location."
  [m [rows cols] [r c]]
  (let [dirs [[-1 0] [0 -1] [0 1] [1 0]]
        nn (map (partial mapv + [r c]) dirs)
        x (m/mget m r c)]
    (filter (fn [[r' c']]
              (and (< -1 r' rows)
                   (< -1 c' cols)
                   (<= (m/mget m r' c') x)))
            nn)))

(defn list-barrels
  "Given a matrix of integers and a starting point, create a list of all the
  elements that can be reached orthogonally that are equal to or less than
  the current value."
  [m start]
  (sch/get-all #(neighbours m (m/shape m) %) 
               (m/ecount m)
               start))

(defn part1
  "Solution for part 1"
  [fname]
  (-> fname
      read-data
      (list-barrels [0 0])
      count))

(defn part2
  "Solution for part 2"
  [fname])

(comment
  (def testf1 "data/q12_p1_test.txt")
  (def inputf1 "data/q12_p1.txt")
  (def testf2 "data/q12_p2_test.txt")
  (def inputf2 "data/q12_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))

;; The End
