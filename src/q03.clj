(ns q03
  (:require [clojure.string :as str]))

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
  [fname]
  (->> fname
       read-data
       set
       vec
       sort
       (take 20)
       (apply +)))

(defn part3
  [fname]
  (->> fname
       read-data
       frequencies
       vals
       (apply max)))

(comment
  (def testf1 "data/q03_p1_test.txt")
  (def inputf1 "data/q03_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q03_p2_test.txt")
  (def inputf2 "data/q03_p2.txt")

  (part2 testf2)
  (part2 inputf2)

  (def testf3 "data/q03_p2_test.txt")
  (def inputf3 "data/q03_p3.txt")

  (part3 testf3)
  (part3 inputf3))

;; The End
