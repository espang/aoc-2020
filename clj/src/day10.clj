(ns day10)

(def input
  (->> (slurp "./aoc/input_10.txt")
       (clojure.string/split-lines)
       (map read-string)))

(def jolts
  (->> input
       (cons 0)
       (cons (+ 3 (apply max input)))
       sort))

(def freqs
  (->>
       jolts
       (partition 2 1)
       (map #(- (second %) (first %)))
       frequencies))
;; part 1
(* (freqs 1) (freqs 3))

;; part 2
(time
 (let [counter (reduce (fn [acc itm]
                         (assoc acc
                                itm
                                (+ (get acc (dec itm) 0)
                                   (get acc (- itm 2) 0)
                                   (get acc (- itm 3) 0))))
                       {0 1}
                       (rest jolts))]
   (println (counter (apply max jolts)))))
