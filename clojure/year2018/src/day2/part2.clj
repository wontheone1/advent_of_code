(ns day2.part2
  (:require [clojure.math.combinatorics :as combo]))

(def input
  (-> (slurp "../../inputs/2018/Day2/input.txt")
      clojure.string/split-lines))

(defn string-differences [str1 str2]
  (->> (map (fn [char1 char2]
              (if (= char1 char2)
                0
                1))
            str1
            str2)
       (reduce +)))

(defn get-common-string-part [str1 str2]
  (apply str (keep-indexed
               (fn [i ch]
                 (when (= ch (get str1 i))
                   ch))
               str2)))

(defn solve [input]
  (loop [[first-combi & rest-combinations] (combo/combinations input 2)]
    (if (= (apply string-differences first-combi) 1)
      (apply get-common-string-part first-combi)
      (recur rest-combinations))))

(defn run [opts]
  (-> input
      solve
      prn))

; run with
; cd year2018/day2
; clj -X part2/run
; => "wlkigsqyfecjqqmnxaktdrhbz"
