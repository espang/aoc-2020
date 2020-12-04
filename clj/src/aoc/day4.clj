(ns day4
  (:require
   [clojure.string :as str]))

(defn parse-pair [p]
  (let [[k v] (str/split p #":")]
    [k v]))

(defn pairs->passport [ps]
  (into {} (map parse-pair ps)))

(def passports
  (let [passports (->
                   (slurp "./aoc/input_4.txt")
                   (str/split #"\n\n"))
        passports (map #(str/split % #"\s|\n") passports)
        passports (map pairs->passport passports)]
    passports))

(def required ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])
(def optional ["cid"])

(defn check-passport [p]
  (every? #(contains? p %) required))

(->> passports
     (filter check-passport)
     count)

(defn number-between [lower upper v]
  (try
    (<= lower (Integer/parseInt v) upper)
    (catch Exception _ false)))

(defn only-digits? [s]
  (every? #(Character/isDigit %) s))

(def validation
  {"byr" (partial number-between 1920 2002)
   "iyr" (partial number-between 2010 2020)
   "eyr" (partial number-between 2020 2030)
   "hgt" (fn [v] (cond
                   (str/ends-with? v "cm")
                   (number-between 150 193
                                   (str/replace v #"(cm)" ""))
                   (str/ends-with? v "in")
                   (number-between 59 76
                                   (str/replace v #"(in)" ""))
                   :else false))
   "hcl" (fn [v] (and (= \# (first v))
                      (= 6 (count (rest v)))
                      (str/blank? (str/replace (apply str (rest v))
                                               #"[0-9a-f]"
                                               ""))))
   "ecl" (fn [v] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                            v))
   "pid" (fn [v] (and (= 9 (count v))
                      (only-digits? v)))})

(defn validate-passport-field [p field]
  (let [validate (get validation field (constantly false))
        value    (get p field "")]
    (validate value)))

(defn check-and-validate-passport [p]
  (every? #(validate-passport-field p %) required))

(->> passports
     (filter check-and-validate-passport)
     count)