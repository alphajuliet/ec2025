(ns q05
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (let [[_ nums] (-> f slurp str/trim-newline (str/split #":"))
        nums' (map Integer/parseInt (str/split nums #","))]
    nums'))

;; Define the fishbone as a vector of nodes. Each node is a vector of length 3.
;; The first example fishbone looks like this:
;;   [[3 5 7] [1 8 10] [5 9 nil] [nil 7 8]]

(def not-nil? (comp not nil?))

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

(defn part1
  "Solution for part 1"
  [fname]
  (let [v (read-data fname)
        fb (reduce
             (fn [t x] (add-number x t 0))
             [[nil (first v) nil]]
             (rest v))]
    (->> fb
         (map second)
         (map str)
         (apply str))))

(defn part2
  "Solution for part 2"
  [fname])

(comment
  (def testf1 "data/q05_p1_test.txt")
  (def inputf1 "data/q05_p1.txt")
  (def testf2 "data/q05_p2_test.txt")
  (def inputf2 "data/q05_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
