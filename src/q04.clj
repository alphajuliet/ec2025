(ns q04
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map Integer/parseInt)))

(defn read-data3
  [f]
  (let [lines (->> f slurp str/split-lines)]
    (->> lines
         (map #(str/split % #"\|"))
         flatten
         (map Integer/parseInt)
         (partition 2)
         (map (partial apply /)))))

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
  [fname]
  (let [coll (read-data fname)
        lcm (apply util/lcm coll)
        [a b] ((juxt first last) (map (partial / lcm) coll))
        r (/ b a)]
    (inc (bigint (/ 10000000000000N r)))))

(defn part3
  [fname]
  (let [coll (read-data3 fname)]
    (->> coll
         (apply *)
         (* 100)
         bigint)))

(comment
  (def testf1 "data/q04_p1_test2.txt")
  (def inputf1 "data/q04_p1.txt")
  (def testf2 "data/q04_p1_test2.txt")
  (def inputf2 "data/q04_p2.txt")
  (def testf3 "data/q04_p3_test.txt")
  (def inputf3 "data/q04_p3.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2)
 
  (part3 testf3)
  (part3 inputf3))
;; The End
