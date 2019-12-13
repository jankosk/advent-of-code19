(ns advent-of-code19.day1
  (:require [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn calculate-fuel [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn fuel-sum []
  (apply
    +
    (map
      (fn [mass] (calculate-fuel (parse-int mass)))
      (str/split-lines (slurp "resources/input/day1.txt")))))


(defn -main [& args]
  (println (fuel-sum)))


