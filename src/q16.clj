(ns q16
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (let [a (-> f 
              slurp 
              (str/trim-newline) 
              (str/split #","))]
    (map Integer/parseInt a)))

(defn part1
  "Solution for part 1"
  [fname]
  (->> fname
       read-data
       (map #(int (/ 90 %)))
       (apply +)))
  
(defn part2
  "Solution for part 2"
  [fname])
  
(def testf1 "data/q16_p1_test.txt")
(def inputf1 "data/q16_p1.txt")
(def testf2 "data/q16_p2_test.txt")
(def inputf2 "data/q16_p2.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
