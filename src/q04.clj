(ns q04
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map Integer/parseInt)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [coll (read-data fname)
        lcm (apply util/lcm coll)
        [a b] ((juxt first last) (map (partial / lcm) coll))]
    (->> (/ b a)
         (* 2025)
         int)))
  
(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q04_p1_test2.txt")
  (def inputf1 "data/q04_p1.txt")
  (def testf2 "data/q04_p2_test.txt")
  (def inputf2 "data/q04_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
