(ns q02
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       (re-seq #"(\d+),(\d+)")
       first
       rest
       (map #(Integer/parseInt %))))

(defn c-add
  [z1 z2]
  (mapv + z1 z2))

(defn c-mul
  "[X1,Y1] * [X2,Y2] = [X1 * X2 - Y1 * Y2, X1 * Y2 + Y1 * X2]"
  [[x1 y1] [x2 y2]]
  [(- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2))])

(defn c-div
  [z1 z2]
  (map quot z1 z2))

(defn a-cycle
  "Run one cycle"
  [r a]
  (-> r
      (c-mul r)
      (c-div [10 10])
      (c-add a)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [a (read-data fname)]
    (reduce 
      (fn [acc _]
        (a-cycle acc a))
      [0 0]
      (range 3))))
  
(defn part2
  "Solution for part 2"
  [fname])
  

(comment
  (def testf1 "data/q02_p1_test.txt")
  (def inputf1 "data/q02_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q02_p2_test.txt")
  (def inputf2 "data/q02_p2.txt")

  (part2 testf2)
  (part2 inputf2))

;; The End
