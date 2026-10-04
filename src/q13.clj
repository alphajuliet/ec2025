(ns q13
  (:require [clojure.string :as str]
            [util :as util]))

(defn reverse-
  "Reverse if a list, otherwise nothing"
  [x]
  (if (seq? (first x))
    (reverse (map reverse x))
    (reverse x)))
 
(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map Integer/parseInt)))

(defn range->list
  "Convert a range string to a list. 
   e.g. (range->list '10-15') => (10 11 12 13 14 15)"
  ;; range->list : String -> List Int
  [range-str]
  (let [[start end] (map Integer/parseInt (str/split range-str #"\-"))]
    (range start (inc end))))

(defn read-data2
  [f]
  (->> f
       slurp
       str/split-lines
       (map range->list)))

(defn assign-numbers
  "Assign numbers to the dial"
  [nums]
  (let [cw (take-nth 2 nums)
        ccw (reverse- (take-nth 2 (rest nums)))]
    (concat cw ccw)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [dial (->> fname
                  read-data
                  assign-numbers
                  (concat '(1)))]
    (->> dial
         count
         (mod 2025)
         (nth dial))))

(defn part2
  "Solution for part 2"
  [fname]
  (let [dial (->> fname
                  read-data2
                  assign-numbers
                  (concat '((1)))
                  (apply concat))]
    (->> dial
         count
         (mod 20252025)
         (nth dial))))
  
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
