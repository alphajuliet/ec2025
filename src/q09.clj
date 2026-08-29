(ns q09
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #":"))
       (map #(update % 0 Integer/parseInt))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [dna (read-data fname)
        m1 (util/count-if true? (map = (second (nth dna 0)) (second (nth dna 2))))
        m2 (util/count-if true? (map = (second (nth dna 1)) (second (nth dna 2))))]
    (* m1 m2)))

(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q09_p1_test.txt")
  (def inputf1 "data/q09_p1.txt")
  (def testf2 "data/q09_p2_test.txt")
  (def inputf2 "data/q09_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
