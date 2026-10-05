(ns q13
  (:require [clojure.string :as str]
            [util :as util]))

(defn reverse-1
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

(defn read-data3
  "Read in the ranges for part 3"
  [f]
  (let [rr (->> f slurp str/split-lines)]
    (map #(map Integer/parseInt (str/split % #"\-")) rr)))

(defn assign-numbers
  "Assign numbers or lists to the dial"
  [nums]
  (let [cw (take-nth 2 nums)
        ccw (reverse-1 (take-nth 2 (rest nums)))]
    (concat cw ccw)))

(defn range-lengths
  [dial]
  (map #(inc (abs (- (second %) (first %)))) dial))

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

(defn find-entry
  "Given a list of lengths of sub-lists, find the list containing the nth number
   and the index into that list. The index n is zero-based into the concatenated
   lists. Returns [list-index offset], or nil if n is out of range."
  ;; find-entry : List Int -> Int -> [Int Int]
  [lengths n]
  (loop [i 0
         n n
         [len & more :as ls] lengths]
    (when (seq ls)
      (if (< n len)
        [i n]
        (recur (inc i) (- n len) more)))))

(defn part3
  "Solution for part 3"
  [fname turns]
  (let [dial (->> fname
                  read-data3
                  assign-numbers)
        lengths (range-lengths dial)
        target (mod turns (inc (apply + lengths)))
        [index offset] (find-entry lengths (dec target))
        [a b] (nth dial index)]
    (if (< a b)
      (+ a offset)
      (- a offset))))

(comment
  (def testf1 "data/q13_p1_test.txt")
  (def inputf1 "data/q13_p1.txt")
  (def testf2 "data/q13_p2_test.txt")
  (def inputf2 "data/q13_p2.txt")
  (def inputf3 "data/q13_p3.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2)

  (part3 testf2 20252025)
  (part3 inputf3 202520252025))
;; The End
