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

(defn find-spell 
  "Find the spell, given the list of block heights"
  [v]
  (->> (reduce 
          (fn [st _]
            (let [st' (extract-factor st)]
              (if (true? (:done st'))
                (reduced st')
                st')))
          {:v v :factors [1]}
          (range 100))
       (:factors)
       butlast))

(defn floor-ratio [r]
  (let [r (rationalize r)
        [n d] (if (ratio? r) [(numerator r) (denominator r)] [r 1])]
    (quot n d)))

(defn ceil-ratio [r]
  (let [r (rationalize r)
        [n d] (if (ratio? r) [(numerator r) (denominator r)] [r 1])]
    (quot (+' n d -1) d)))

(defn floor-sum-inverse
  "Largest integer m with Σ⌊m/a⌋ <= s, for positive integers `as`."
  [as s]
  (let [S  (fn [m] (transduce (map #(quot m %)) +' 0 as))
        H  (reduce + (map #(/ 1 %) as))              ; exact Ratio
        K  (count as)]
    (loop [lo (floor-ratio (/ s H))                  ; S(lo) <= s guaranteed
           hi (ceil-ratio  (/ (+ s K) H))]           ; S(hi) >  s guaranteed
      (if (< lo hi)
        (let [mid (quot (+' lo hi 1) 2)]             ; upper mid, so the loop terminates
          (if (<= (S mid) s)
            (recur mid hi)
            (recur lo (dec mid))))
        lo))))

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
    (->> v
         find-spell
         (apply *))))

(defn part3
  "Solve part 3, with some help on the maths from Claude."
  [fname n]
  (-> fname
      read-data
      vec
      find-spell
      (floor-sum-inverse n)))
  
(def testf1 "data/q16_p1_test.txt")
(def inputf1 "data/q16_p1.txt")
(def testf2 "data/q16_p2_test.txt")
(def inputf2 "data/q16_p2.txt")
(def inputf3 "data/q16_p3.txt")

(comment
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2)
  
  (part3 testf2 100000000)
  (part3 inputf3 202520252025000))
;; The End
