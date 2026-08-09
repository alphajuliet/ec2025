(ns q01
  (:require [clojure.string :as str]))

(defn read-data
  "Read the data and return a vector of the words and the actions."
  ;; String -> [String, String]
  [f]
  (let [[a b] (->> f
                   slurp
                   str/split-lines
                   (split-with (comp not empty?)))
        words (str/split (first a) #",")
        actions (str/split (last b) #",")]
    [words actions]))

(defn clamp
  "Clamp value of n to the range [a, b]"
  ;; clamp : Number -> Number -> Number -> Number
  [a b n]
  (-> n (max a) (min b)))

(defn swap-elements
  "Swap two elements of a collection"
  [coll i j]
  (assoc coll j (nth coll i) i (nth coll j)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [[words actions] (read-data fname)
        index (reduce (fn [acc e]
                        (let [dir (first e)
                              x (Integer/parseInt (subs e 1))
                              n (dec (count words))]
                          (case dir
                            \L (clamp 0 n (- acc x))
                            \R (clamp 0 n (+ acc x)))))
                      0
                      actions)] 
    (nth words index)))

(defn part2
  "Solution for part 2"
  [fname]
  (let [[words actions] (read-data fname)
        index (reduce (fn [acc e]
                        (let [dir (first e)
                              x (Integer/parseInt (subs e 1))
                              n (count words)]
                          (case dir
                            \L (mod (- acc x) n)
                            \R (mod (+ acc x) n))))
                      0
                      actions)] 
    (nth words index)))

(defn part3
  "Solution for part 3"
  [fname]
  (let [[words actions] (read-data fname)
        n (count words)
        indices (reduce (fn [acc e]
                          (let [dir (first e)
                                x (Integer/parseInt (subs e 1))]
                            (case dir
                              \L (swap-elements acc 0 (mod (- x) n))
                              \R (swap-elements acc 0 (mod x n)))))
                        (vec (range n))
                        actions)] 
    (nth words (first indices))))

(comment
  (def testf1 "data/q01_p1_test.txt")
  (def inputf1 "data/q01_p1.txt")

  (def testf2 "data/q01_p1_test.txt")
  (def inputf2 "data/q01_p2.txt")

  (def testf3 "data/q01_p3_test.txt")
  (def inputf3 "data/q01_p3.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2)

  (part3 testf3)
  (part3 inputf3))

;; The End
