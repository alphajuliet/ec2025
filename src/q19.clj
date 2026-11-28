(ns q19
  (:require [clojure.string :as str]
            [util :as util]
            [search :as sch]))

(defn read-data
  "Return the locations of the walls"
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #","))
       (util/mapmap Integer/parseInt)))

(defn gap?
  "True if [x y] is passable: column x has no wall, or y lies in one of its gaps.
   Each gap is [x y-start height]."
  [gaps [x y]]
  (let [in-column?  (fn [[gx _ _]] (= gx x))
        covers-y?   (fn [[_ y0 h]] (< (dec y0) y (+ y0 h)))
        column-gaps (filter in-column? gaps)]
    (or (empty? column-gaps)
        (boolean (some covers-y? column-gaps)))))

(defn next-steps
  "Return the options: flap up or coast down"
  [walls xy]
  (let [delta [[1 1] [1 -1]]
        nn (mapv #(mapv + % xy) delta)] 
    (filter #(and (nat-int? (second %)) 
                  (gap? walls %))
            nn)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [gaps (read-data fname)
        lastx (first (last gaps))
        soln (sch/shortest-path-1 (partial next-steps gaps)
                                  (fn [curr child]
                                    (if (< (second child) (second curr)) 0 1))
                                  1e6
                                  [0 0]
                                  #(>= (first %) lastx))]
    (->> soln
         second
         (filter #(and (= (first (key %)) lastx)
                       (gap? gaps (first %))))
         (sort-by val)
         first
         val)))

(def part2 part1)
(def part3 part1)

(def testf1 "data/q19_p1_test.txt")
(def inputf1 "data/q19_p1.txt")
(def testf2 "data/q19_p2_test.txt")
(def inputf2 "data/q19_p2.txt")
(def inputf3 "data/q19_p3.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2)

  (part3 inputf3))
;; The End
