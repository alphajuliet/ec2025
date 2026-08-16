(ns q03
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  "Read the input data"
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #","))
       first
       (map Integer/parseInt)))


(defn part1
  "Solution for part 1"
  [fname]
  (->> fname
       read-data
       set
       (apply +)))

(defn part2
  "Solution for part 2"
  [fname])

(comment
  (def testf1 "data/q03_p1_test.txt")
  (def inputf1 "data/q03_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q03_p2_test.txt")
  (def inputf2 "data/q03_p2.txt")

  (part2 testf2)
  (part2 inputf2))
;; The End
