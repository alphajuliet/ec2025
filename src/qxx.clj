(ns qxx
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp))

(defn part1
  "Solution for part 1"
  [fname]
  (read-data fname))
  
(defn part2
  "Solution for part 2"
  [fname]
  (read-data fname))
  
(def testf1 "data/qxx_p1_test.txt")
(def inputf1 "data/qxx_p1.txt")
(def testf2 "data/qxx_p2_test.txt")
(def inputf2 "data/qxx_p2.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
