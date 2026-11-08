(ns q17
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (let [m (->> f
               slurp
               str/split-lines
               (map #(str/split % #"")))
        [x0 y0] (first (util/mfind-all m "@"))]
    {:m (util/mapmap Integer/parseInt (m/mset m x0 y0 "0")) 
     :org [x0 y0] 
     :shape (m/shape m)}))

(defn inside?
  "Is a given point within radius `r` of the origin"
  [[x0 y0] [xc yc] r]
  (let [dx (- x0 xc)
        dy (- y0 yc)]
    (<= (+ (* dx dx)
           (* dy dy))
        (* r r))))

(defn area-mask
  "Create a boolean mask of points within a given radius"
  [m org r]
  (m/emap-indexed 
    (fn [coord _]
      (if (inside? org coord r) 1 0)) 
    m))

(defn border-mask
  "Iterate one radius increment"
  [m org r]
  (let [mask1 (area-mask m org r)
        mask0 (area-mask m org (dec r))]
    (m/sub mask1 mask0)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [{:keys [m org]} (read-data fname)
        mask (area-mask m org 10)]
    (->> m
         (m/mul mask)
         m/esum)))

(defn part2
  "Solution for part 2"
  [fname]
  (let [{:keys [m org shape]} (read-data fname)
        scores (->> (range 1 (first shape))
                    (map #(-> m 
                           (m/mul (border-mask m org %)) 
                           m/esum))) 
        a (apply max scores)      
        i (.indexOf scores a)]
    (* a (inc i))))
  
(def testf1 "data/q17_p1_test.txt")
(def inputf1 "data/q17_p1.txt")
(def testf2 "data/q17_p2_test.txt")
(def inputf2 "data/q17_p2.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
