(ns q08
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (-> f
      slurp
      str/trim-newline
      (str/split #",")
      (#(map Integer/parseInt %))))

(defn part1
  "Solution for part 1"
  [fname n]
  (let [v (read-data fname)]
    (->> v
         (map #(mod % (/ n 2)))
         (partition 2 1)
         (filter #(= (first %) (second %)))
         count)))

(defn part2
  "Solution for part 2"
  [fname])

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
