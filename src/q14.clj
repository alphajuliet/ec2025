(ns q14
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]
            [util :as util]))

(defn read-data
  "Read the data and convert into an integer matrix"
  [f]
  (->> f
       slurp
       str/split-lines
       (util/mapmap #(str/replace % #"\#" "1"))
       (util/mapmap #(str/replace % #"\." "0"))
       (util/mapmap Integer/parseInt)))

(defn neighbours
  "Get the diagonal neighbours and the centre element."
  [[rows cols] rc]
  (let [diags [[-1 -1] [-1 1] [1 -1] [1 1] [0 0]]
        nn (map (partial mapv + rc) diags)]
    (filter (fn [[r' c']]
              (and (< -1 r' rows)
                   (< -1 c' cols)))
            nn)))

(defn update-elt
  "Update an element"
  [m shape rc]
  (let [sum (apply + (mapv #(apply m/mget m %) (neighbours shape rc)))]
    (if (odd? sum)
      0 
      1)))

(defn transform
 [m shape]
 (m/emap-indexed (fn [idx _] (update-elt m shape idx)) m))

(defn part1
  "Solution for part 1"
  [fname]
  (let [m (read-data fname)
        shape (m/shape m)]
    (->> (reductions
           (fn [s _] (transform s shape))
           m
           (range 10))
         rest
         (map m/esum)
         (apply +))))

(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q14_p1_test.txt")
  (def inputf1 "data/q14_p1.txt")
  (def testf2 "data/q14_p2_test.txt")
  (def inputf2 "data/q14_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
