(ns part1
  (:require [clojure.string]))

(def input
  (->
   (slurp "../../../inputs/2018/Day4/input.txt")
   clojure.string/split-lines)
  )

(comment
 input
 )

(defn parse-line [line]
  (let [timestamp-date-part (re-find #"\d{4}-\d{2}-\d{2}" line)
        timestamp-minute (Integer/parseInt (-> (re-find #"\d{2}:\d{2}" line)
                                               (clojure.string/split #":")
                                               (second)))]
    {:guard-no (Integer/parseInt (-> (re-find #"#\d+" line)
                                     (subs 1)))
     :timestamp-date-part timestamp-date-part
     :timestamp-hour-minute timestamp-minute}))

(comment
 (parse-line "[1518-11-01 00:05] Guard #10 begins shift"))
