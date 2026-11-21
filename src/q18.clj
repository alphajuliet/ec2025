(ns q18
  (:require [ubergraph.core :as uber]
            [instaparse.core :as insta]
            [util :as util]))

(def plants
  (insta/parser
    "<S> := (Plant|<NL>)+
     NL := '\n'
     Plant := Defn Branch+
     <Defn> := <'Plant '> Node <' with thickness '> Thickness <':'> <NL>
     <Branch> := (Free | Connect) <NL>
     Free := <'- free branch with thickness '> Thickness
     Connect := <'- branch to Plant '> Node <' with thickness '> Thickness
     Thickness := Number
     Node := Number
     <Number> := #'\\d+'"))

(defn read-data
  [f]
  (->> f
       slurp 
       (insta/parse plants)
       (insta/transform 
         {:Plant (fn [n t & c] 
                   (conj (map #(vector n (first %) (second %)) c) 
                         [n {:thickness t}]))
          :Node Integer/parseInt
          :Thickness Integer/parseInt
          :Free #(vector 0 %1)
          :Connect vector})
      (apply concat)
      (concat [[0 {:thickness 1}]])))

(defn plant-thickness
  [g node]
  (-> g
      (uber/node-with-attrs node)
      second
      :thickness))

(defn energy
  "Calculate the energy through the graph"
  [g start]
  (let [pth (plant-thickness g start)
        succ (uber/successors g start)
        en-in (if (nil? succ)
                pth
                ;; else
                (reduce
                  (fn [acc n] 
                    (let [en (energy g n)
                          bth (uber/weight g [start n])]
                      (+ acc (* bth en))))
                  0
                  succ))]
     (if (< en-in pth)
       0
       en-in)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [d (->> fname read-data)
        g (apply uber/digraph d)
        last-node (apply max (uber/nodes g))]
    (energy g last-node)))
  
(defn part2
  "Solution for part 2"
  [fname])
  
(def testf1 "data/q18_p1_test.txt")
(def inputf1 "data/q18_p1.txt")
(def testf2 "data/q18_p2_test.txt")
(def inputf2 "data/q18_p2.txt")

(comment
  (read-data testf1)
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
