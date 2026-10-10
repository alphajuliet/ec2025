(ns q15
  (:require [clojure.string :as str]
            [search :as sch]))

(defn decode
  "Decode the instruction string"
  [s]
  (let [dir (subs s 0 1)
        dist (Integer/parseInt (subs s 1))]
    [dir dist]))

(defn read-data
  [f]
  (-> f
      slurp
      str/trim-newline
      (str/split #",")
      (#(mapv decode %))))

(defn turn-left [[x y]] [(- y) x])
(defn turn-right [[x y]] [y (- x)])
(defn vscale [k v] (map (partial * k) v))

(defn get-corners
  "Given the instructions, create a list of corners"
  [instrs]
  (reduce 
    (fn [st [dir dist]]
      (case dir
        "L" (-> st 
                (update :heading turn-left)
                (as-> st' (update st' :xy #(mapv + % (vscale dist (:heading st')))))
                (as-> st' (update st' :corners conj (:xy st'))))
        "R" (-> st 
                (update :heading turn-right)
                (as-> st' (update st' :xy #(mapv + % (vscale dist (:heading st')))))
                (as-> st' (update st' :corners conj (:xy st'))))))
    {:corners []
     :xy [0 0]
     :heading [0 1]}
    instrs))

(defn derive-edges
  "Given two vertices, derive all the points in between."
  [[x1 y1] [x2 y2]]
  (cond 
    (and (= x1 x2) (< y1 y2)) (map vector (repeat x1) (range y1 (inc y2)))
    (and (= x1 x2) (> y1 y2)) (map vector (repeat x1) (range y1 (dec y2) -1))
    (and (= y1 y2) (< x1 x2)) (map vector (range x1 (inc x2)) (repeat y1))
    (and (= y1 y2) (> x1 x2)) (map vector (range x1 (dec x2) -1) (repeat y1))
    :else :error))

(defn create-boundary
  "Generate all the boundary points given the instructions."
  [instrs]
  (let [cp (partition 2 1 (:corners (get-corners instrs)))]
    (mapcat (partial apply derive-edges) cp)))

(defn minmax
  "Get the range of values on each axis"
  [pts]
  (let [[xs ys] (apply map vector pts)]
    [(apply min xs) (apply max xs)
     (apply min ys) (apply max ys)]))

(defn neighbours
  "Get all the orthogonal neighbour coords not on the boundary"
  [boundary [xmin xmax ymin ymax] [x y]]
  (let [dirs [[0 1] [1 0] [-1 0] [0 -1]]
        nn (map (partial mapv + [x y]) dirs)]
    (filter (fn [[x' y']] 
              (and (<= xmin x' xmax)
                   (<= ymin y' ymax)
                   (not (contains? boundary [x' y']))))
            nn)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [bb (->> fname
                read-data
                create-boundary)
        dims (minmax bb)
        end (last bb)]
    (-> (sch/shortest-path (partial neighbours (disj (set bb) end) dims)
                           (constantly 1) 
                           1000 
                           [0 0]
                           end)
        second
        (get end))))

(defn part2
  "Solution for part 2"
  [fname]
  (read-data fname))

(def testf1 "data/q15_p1_test.txt")
(def inputf1 "data/q15_p1.txt")
(def testf2 "data/q15_p2_test.txt")
(def inputf2 "data/q15_p2.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))

;; The End
