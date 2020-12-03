(ns aoc.day1
  (:require
   [clojure.string :as str]))

(def numbers 
  (->> (slurp "./aoc/input_1.txt")
       (str/split-lines)
       (map #(Integer/parseInt %))
       (into #{})))

(defn find-pair [numbers sum]
  (first
   (for [x numbers :when (contains? numbers (- sum x))]
     [x (- sum x)])))

(defn product [coll]
  (reduce * 1 coll))

(println "part1: "
         (product (find-pair numbers 2020)))

(defn find-triple [numbers sum]
  (first
   (for [x numbers :when (find-pair (disj numbers x)
                                    (- sum x))]
     (let [[y z] (find-pair (disj numbers x) (- sum x))]
       [x y z]))))

(println "part2: "
         (product (find-triple numbers 2020)))
