(ns day10)

(def input
  (->> (slurp "./aoc/input_10.txt")
       (clojure.string/split-lines)
       (map read-string)))

(defn jolts [jolts]
  (->> jolts
       (cons 0)
       (cons (+ 3 (apply max jolts)))
       sort))

((def freqs
   (->> input
        jolts
        (partition 2 1)
        (map #(- (second %) (first %)))
        frequencies))
;part 1
 (* (freqs 1) (freqs 3))

 (let [lookup  (into #{} (jolts input))
       start   (apply max lookup)
       counter (reduce (fn [acc itm]
                         (assoc acc
                                itm
                                (reduce + (map #(get acc % 0) ))))
                       {0 1}
                       (rest (sort lookup)))]))

(range 0 -3)

(-> input
    jolts
    combinations)
