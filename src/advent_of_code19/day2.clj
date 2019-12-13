(ns advent-of-code19.day2
  [:require [clojure.string :as str]])

(def input
  (mapv
    #(Integer/parseInt %)
    (str/split (slurp "resources/input/day2.txt") #"(,|\s)")))

(defn operation [vec i]
  (let [op (get vec i)
        idx1 (get vec (+ i 1))
        idx2 (get vec (+ i 2))
        idx3 (get vec (+ i 3))
        first (get vec idx1)
        second (get vec idx2)]
    (cond
      (= op 1) (assoc vec idx3 (+ first second))
      (= op 2) (assoc vec idx3 (* first second))
      (= op 99) (reduced vec))))

(defn initialise-input [input noun verb]
    (assoc input 1 noun 2 verb))

(defn run-code [input noun verb]
  (let [int-code (initialise-input input noun verb)
        size (count int-code)]
    (first
      (reduce #(operation %1 %2) int-code (range 0 size 4)))))

(defn find-output [output]
  (let [results (for [noun (range 0 100) verb (range 0 100)]
                  {:noun noun :verb verb :result (run-code input noun verb)})
        result (first (filter #(= output (:result %)) results))]
    (+ (* 100 (:noun result)) (:verb result))))

(defn -main [& args]
  (println (run-code input 12 2))
  (println (find-output 19690720)))
