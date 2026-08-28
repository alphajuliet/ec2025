(ns q08
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [util :as util]))

(defn read-data
  [f]
  (-> f
      slurp
      str/trim-newline
      (str/split #",")
      (#(map Integer/parseInt %))))

(defn xor
  [a b]
  (or (and a (not b))
      (and b (not a))))

(defn intersects?
  "Do the chords between the pairs of points intersect?"
  [[a b] [c d]]
  (if (or (= b c) (= a d) (= a c) (= b d))
    false
    (let [[lo hi] ((juxt min max) a b)]
      (xor (< lo c hi) (< lo d hi)))))

(defn part1
  "Solution for part 1"
  [fname n]
  (->> fname
       read-data
       (map #(mod % (/ n 2)))
       (partition 2 1)
       (filter #(= (first %) (second %)))
       count))

(defn part2
  "Solution for part 2"
  [fname]
  (let [t (->> fname read-data (partition 2 1))
        pairs (combo/combinations t 2)]
    (->> pairs
        (filter #(apply intersects? %))
        count)))

(comment
  (def testf1 "data/q08_p1_test.txt")
  (def inputf1 "data/q08_p1.txt")
  (def testf2 "data/q08_p2_test.txt")
  (def inputf2 "data/q08_p2.txt")

  (part1 testf1 8)
  (part1 inputf1 32)

  (part2 testf2)
  (part2 inputf2))

;; The End
