(ns q06
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/trim-newline))

(defn count-x-before-y 
  "Count the occurrences of x before y in a string s"
  [x y s]
  (->> s
       (reduce 
         (fn [[total x-count] ch]
           (condp = ch
             x [total (inc x-count)]
             y [(+ total x-count) x-count]
             [total x-count]))
         [0 0])
       first))

(defn part1
  "Solution for part 1"
  [fname]
  (->> fname
       read-data
       (count-x-before-y \A \a)))
  

(defn part2
  "Solution for part 2"
  [fname]
  (let [s (read-data fname)
        a (count-x-before-y \A \a s)
        b (count-x-before-y \B \b s)
        c (count-x-before-y \C \c s)]
    (+ a b c)))
  
(comment
  (def testf1 "data/q06_p1_test.txt")
  (def inputf1 "data/q06_p1.txt")
  (def testf2 "data/q06_p1_test.txt")
  (def inputf2 "data/q06_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
