(ns q16
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [f]
  (let [a (-> f 
              slurp 
              (str/trim-newline) 
              (str/split #","))]
    (map Integer/parseInt a)))

(defn dec-every-nth 
  "Decrement every nth element in the collection but not below zero."
  [n v]
  (reduce #(update %1 %2 (fn [x] (max 0 (dec x))))
          v
          (range (dec n) (count v) n)))

(defn extract-factor
  "Extract the next factor from the vector v, given the current spacing n"
  [{:keys [v] :as st}]
  (let [nz (.indexOf v 1)]
    (if (neg-int? nz)
      (into st {:done true})
      ;; else
      (let [v' (dec-every-nth (inc nz) v)
            idx (inc (count (take-while zero? v')))]
        (-> st
            (assoc :v v')
            (update :factors conj idx))))))

(defn part1
  "Solution for part 1"
  [fname]
  (->> fname
       read-data
       (map #(int (/ 90 %)))
       (apply +)))
  
(defn part2
  "Solution for part 2"
  [fname]
  (let [v (->> fname read-data vec)]
    (->> (reduce 
          (fn [st _]
            (let [st' (extract-factor st)]
              (if (true? (:done st'))
                (reduced st')
                st')))
          {:v v :factors [1]}
          (range 100))
         (:factors)
         butlast
         (apply *))))
  
(def testf1 "data/q16_p1_test.txt")
(def inputf1 "data/q16_p1.txt")
(def testf2 "data/q16_p2_test.txt")
(def inputf2 "data/q16_p2.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
