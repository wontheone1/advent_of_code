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
                                               (second)))
        kind (cond
               (re-find #"Guard" line)
               :change-shift

               (re-find #"falls" line)
               :falls-asleep

               (re-find #"wakes" line)
               :wakes-up)]
    (merge
     {:kind kind
      :timestamp-date-part timestamp-date-part
      :timestamp-minute timestamp-minute}
     (when (= kind :change-shift)
       {:guard-no (Integer/parseInt (-> (re-find #"#\d+" line)
                                        (subs 1)))}))))

(comment
 (parse-line "[1518-11-01 00:05] Guard #10 begins shift")
 (parse-line "[1518-11-01 00:10] falls asleep")
 (parse-line "[1518-11-01 00:25] wakes up"))

