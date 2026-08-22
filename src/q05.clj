(ns q05
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (let [[_ nums] (-> f slurp str/trim-newline (str/split #":"))
        nums' (map Integer/parseInt (str/split nums #","))]
    nums'))

(defn read-data2
  "Read the data for part 2"
  [f]
  (let [swords (->> f slurp str/split-lines) 
        lines (map #(str/split % #":") swords)
        ids (map (comp Integer/parseInt first) lines)
        nums (->> lines
              (map second)
              (map #(str/split % #","))
              (util/mapmap Integer/parseInt))]
     (map vector ids nums))) 

;; The first example fishbone looks like this:
;;   [[3 5 7] [1 8 10] [5 9 nil] [nil 7 8]]
(defn add-number
  "Recursively add a number x to the fishbone"
  [x fb i]
  (let [[a b c] (nth fb i)]
    (cond 
      (and (< x b) (nil? a)) (assoc fb i [x b c])
      (and (> x b) (nil? c)) (assoc fb i [a b x])
      ;; if we're at the end then add a new node
      (= i (dec (count fb))) (conj fb [nil x nil])
      ;; else try and fit x in the next node
      :else (add-number x fb (inc i)))))

(defn create-fb
  "Create a fishbone from a collection of numbers"
  [v]
  (reduce
    (fn [t x] (add-number x t 0))
    [[nil (first v) nil]]
    (rest v)))

(defn quality
  "Get the quality from the fishbone"
  [fb]
  (->> fb
       (map second)
       (map str)
       (apply str)
       bigint))

(defn node-value
  "Convert a node vector into a decimal"
  [node]
  (->> node
       (remove nil?)
       (apply str)
       Integer/parseInt))

(defn values
  "Get the values of a fishbone"
  [fb]
  (->> fb
       (mapv node-value)))

(defn coll-range
  "Calculate the range of the collection"
  [coll]
  (- (apply max coll) (apply min coll)))

(defn part1
  "Solution for part 1"
  [fname]
  (->> fname
       read-data
       create-fb
       quality))

(defn part2
  "Solution for part 2"
  [fname]
  (->> fname
       read-data2
       (map second)
       (map (comp bigint quality create-fb))
       coll-range))

(defn part3
  [fname]
  (let [data (read-data2 fname)
        ids (map first data)
        fbs (map (comp create-fb second) data)
        levels (map values fbs)
        qualities (map quality fbs)]
    (->> (map list ids levels qualities)
         (sort-by (juxt util/third second first))
         (map first)
         reverse
         (map * (range 1 (inc (count ids))))
         (apply +))))

(comment
  (def testf1 "data/q05_p1_test.txt")
  (def inputf1 "data/q05_p1.txt")
  (def testf2 "data/q05_p2_test.txt")
  (def inputf2 "data/q05_p2.txt")
  (def testf3 "data/q05_p3_test.txt")
  (def inputf3 "data/q05_p3.txt")

  (part1 testf1)
  (part1 inputf1)
  (part2 testf2)
  (part2 inputf2)
  (part3 testf3)
  (part3 inputf3))

;; The End
