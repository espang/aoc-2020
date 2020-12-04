
(ns day2
  (:require
   [clojure.string :as str]))

(defn parse-line [l]
  (let [[numbers character password] (str/split l #" ")
        [n1 n2] (->> (str/split numbers #"-")
                     (map read-string))]
    [[n1 n2] (first character) password]))

(def input 
  (-> (slurp "./aoc/input_2.txt")
      (str/split-lines)))

(defn test-input [test-fn]
  (->> input
       (map parse-line)
       (filter test-fn)
       count))

;; part 1
(defn test1 [[[n1 n2] c pw]]
  (let [occurences (get (frequencies pw) c 0)]
    (<= n1 occurences n2)))

(println "part1: "
         (test-input test1))

;; part2
(defn test2 [[[n1 n2] c pw]]
  (let [c1 (nth pw (dec n1))
        c2 (nth pw (dec n2))]
    (or (and (= c c1) (not= c c2))
        (and (not= c c1) (= c c2)))))

(println "part2: "
         (test-input test2))
