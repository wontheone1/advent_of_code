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

(defn aggregate-sleep-records [sorted-parsed-lines]
  (reduce
   (fn [agg {:keys [guard-no kind timestamp-date-part timestamp-minute] :as _parsed-line}]
     (case kind
       :change-shift
       (merge
        agg
        {:current-date timestamp-date-part
         :current-guard-no guard-no
         :current-sleep-start nil})

       :falls-asleep
       (merge
        agg
        {:current-date timestamp-date-part
         :current-sleep-start timestamp-minute})

       :wakes-up
       (merge
        agg
        {
         ; :current-date timestamp-date-part (Data shows that guards don't sleep overnight. they only sleep from 00:00 ~ 01:00)
         ; :current-sleep-start nil (if :falls-asleep and :wakes-up alternate as expected, then no need to clean up)
         :aggregated-sleep-record (conj (:aggregated-sleep-record agg)
                                        {:guard-no (:current-guard-no agg)
                                         :date (:current-date agg)
                                         :sleep-start-time (:current-sleep-start agg)
                                         :sleep-end-time timestamp-minute})})))
   {:current-date nil
    :current-guard-no nil
    :current-sleep-start nil
    :aggregated-sleep-record []}
   sorted-parsed-lines))

(defn solve [input]
  (->> input
       sort
       (map parse-line)
       aggregate-sleep-records))

(comment
 (solve input))
