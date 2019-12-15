(ns advent-of-code19.day5
  [:require [clojure.string :as str]])

(def input
  (mapv
    #(Integer/parseInt %1)
    (str/split (slurp "resources/input/day5.txt") #"(,|\s)")))

(defn get-op-code [vec idx]
  (let [op (get vec idx)]
    (if (> (count (str op)) 1)
      (Integer/parseInt (str/join (take-last 2 (str op))))
      op)))

(defn get-input []
  (do
    (print "Set value: ")
    (flush)
    (Integer/parseInt (read-line))))

(defn operation [vec i param1 param2]
  (let [op (get-op-code vec i)
        pos1 (get vec (+ i 1))
        pos2 (get vec (+ i 2))
        pos3 (get vec (+ i 3))
        val1 (if (= param1 0) (get vec pos1) pos1)
        val2 (if (= param2 0) (get vec pos2) pos2)]
    (cond
      (= op 1) (assoc vec pos3 (+ val1 val2))
      (= op 2) (assoc vec pos3 (* val1 val2))
      (= op 3) (assoc vec pos1 (get-input))
      (= op 4) (do (println val1) vec)
      :else vec)))

(defn multi-operation [int-code idx]
  (let [multi-op (str (get int-code idx))
        op-vec (str/split multi-op #"")
        size (count op-vec)
        param1 (Integer/parseInt (nth op-vec (- size 3)))
        param2 (if (< (- size 4) 0) 0 (Integer/parseInt (nth op-vec (- size 4))))]
    (operation int-code idx param1 param2)))

(defn handle-operation [int-code idx]
  (let [is-multi-op (> (count (str (get int-code idx))) 1)]
    (if is-multi-op
      (multi-operation int-code idx)
      (operation int-code idx 0 0))))

(defn get-next-idx [idx op]
  (cond
    (or (= op 1) (= op 2)) (+ idx 4)
    (or (= op 3) (= op 4)) (+ idx 2)
    :else (+ idx 1)))

(defn run-code [input]
  (loop [int-code input idx 0]
    (let [op (get-op-code int-code idx)]
      (if (or (= op 99) (>= idx (count int-code)))
        int-code
        (recur (handle-operation int-code idx) (get-next-idx idx op))))))

(defn -main [& args]
  (println (run-code input)))