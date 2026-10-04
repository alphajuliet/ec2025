(ns q13
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map Integer/parseInt)))

(defn assign-numbers
  "Assign numbers to the dial"
  [nums]
  (let [cw (take-nth 2 nums)
        ccw (reverse (take-nth 2 (rest nums)))]
    (concat '(1) cw ccw)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [dial (->> fname
                  read-data
                  assign-numbers)]
    (->> dial
         count
         (mod 2025)
         (nth dial))))

(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q13_p1_test.txt")
  (def inputf1 "data/q13_p1.txt")
  (def testf2 "data/q13_p2_test.txt")
  (def inputf2 "data/q13_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
