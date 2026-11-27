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

(defn is-wall?
  "Given a coordinate, is it a wall?"
  [walls [x y]]
  (let [thisx (filter #(= (first %) x) walls)]
    (if (empty? thisx)
      false
      ;; else
      (let [[_ y1 y2] (first thisx)]
        (not (<= y1 y (dec (+ y1 y2))))))))

(defn next-steps
  "Return the options: flap up or coast down"
  [walls xy]
  (let [delta [[1 1] [1 -1]]
        nn (mapv #(mapv + % xy) delta)] 
    (filter #(and (nat-int? (second %)) 
                  (not (is-wall? walls %)))
            nn)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [walls (read-data fname)
        lastx (first (last walls))
        soln (sch/shortest-path-1 (partial next-steps walls)
                                  (fn [curr child]
                                    (if (< (second child) (second curr)) 0 1))
                                  1000
                                  [0 0]
                                  #(>= (first %) lastx))]
    (->> soln
         second
         (filter #(and (= (first (key %)) lastx)
                       (not (is-wall? walls (first %)))))
         (sort-by val)
         first
         val)))

(defn part2
  "Solution for part 2"
  [fname]
  (read-data fname))
  
(def testf1 "data/q19_p1_test.txt")
(def inputf1 "data/q19_p1.txt")
(def testf2 "data/q19_p2_test.txt")
(def inputf2 "data/q19_p2.txt")

(comment
  (part1 testf1)
  (part1 inputf1)
  ;; not 51

  (part2 testf2)
  (part2 inputf2))

;; The End
