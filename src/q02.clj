(ns q02)

(defn read-data
  [f]
  (->> f
       slurp
       (re-seq #"A=\[(-?\d+),(-?\d+)\]")
       first
       rest
       (mapv #(Integer/parseInt %))))

(defn c-add
  "Complex add"
  [z1 z2]
  (mapv + z1 z2))

(defn c-mul
  "[X1,Y1] * [X2,Y2] = [X1 * X2 - Y1 * Y2, X1 * Y2 + Y1 * X2]"
  [[x1 y1] [x2 y2]]
  [(- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2))])

(defn c-div
  "Scalar division"
  [z1 z2]
  (map quot z1 z2))

(defn cycle-once
  "Run one cycle"
  [r a d]
  (-> r
      (c-mul r)
      (c-div d)
      (c-add a)))

(defn out-of-bounds?
  [[x y]]
  (or (> (abs x) 1000000)
      (> (abs y) 1000000)))

(defn engrave?
  "Does this point get engraved?"
  [z]
  (let [x (reduce
            (fn [acc _]
              (if (out-of-bounds? acc)
                (reduced [1000001 1000001])
                (cycle-once acc z [100000 100000])))
            [0 0]
            (range 100))]
    (not (out-of-bounds? x))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [a (read-data fname)]
    (reduce 
      (fn [acc _] (cycle-once acc a [10 10]))
      [0 0]
      (range 3))))

(defn part2
  "Solution for part 2"
  [fname]
  (let [a (read-data fname)
        m (for [i (range 101)
                j (range 101)]
            (c-add a [(* i 10) (* j 10)]))]
    (->> m
         (filter engrave?)
         count)))

(defn part3
  "Solution for part 3"
  [fname]
  (let [a (read-data fname)
        m (for [i (range 1001)
                j (range 1001)]
            (c-add a [i j]))]
    (->> m
         (filter engrave?)
         count)))

(comment
  (def testf1 "data/q02_p1_test.txt")
  (def inputf1 "data/q02_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q02_p2_test.txt")
  (def inputf2 "data/q02_p2.txt")

  (part2 testf2)
  (part2 inputf2)

  (def testf3 "data/q02_p3_test.txt")
  (def inputf3 "data/q02_p3.txt")

  (time (part3 testf3))
  (time (part3 inputf3)))

;; The End
