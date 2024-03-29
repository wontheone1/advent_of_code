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
         ; :current-date timestamp-date-part (Data shows that guards don't sleep overnight. they only sleep from 00:00 ~ 00:59)
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

(defn group-sleep-records-by-guard-id [aggregated-sleep-records]
  (group-by :guard-no aggregated-sleep-records))

(def sleep-minute-frequency-tracker
  (vec (repeat 60 0)))

(defn reflect-sleep-record [sleep-minute-tracker sleep-record]
  (reduce (fn [sleep-minute-tracker slept-minute]
            (update sleep-minute-tracker slept-minute inc))
          sleep-minute-tracker
          (range (:sleep-start-time sleep-record)
                 (:sleep-end-time sleep-record))))

(comment
 (reflect-sleep-record
  sleep-minute-frequency-tracker {:guard-no 2953, :date "1518-03-19", :sleep-start-time 37, :sleep-end-time 41}))

(defn reflect-sleep-records [sleep-minute-tracker sleep-records]
  (reduce (fn [sleep-minute-tracker sleep-record]
            (reflect-sleep-record sleep-minute-tracker sleep-record))
   sleep-minute-tracker
   sleep-records))

(comment
 (reflect-sleep-records
  sleep-minute-frequency-tracker
  [{:guard-no 2953, :date "1518-03-19", :sleep-start-time 37, :sleep-end-time 41}
   {:guard-no 2953, :date "1518-04-09", :sleep-start-time 31, :sleep-end-time 47}]))

(defn solve [input]
  (let [sleep-records-by-guard-id (->> input
                                       sort
                                       (map parse-line)
                                       aggregate-sleep-records
                                       :aggregated-sleep-record
                                       group-sleep-records-by-guard-id)
        total-sleep-minutes-by-guard-id (map (fn [[k v]]
                                               {:guard-id k
                                                :total-sleep-minute (apply + (map
                                                                              (fn [{:keys [sleep-start-time
                                                                                           sleep-end-time]}]
                                                                                (- sleep-end-time sleep-start-time))
                                                                              v))})
                                             sleep-records-by-guard-id)
        guard-id-slept-most (:guard-id (apply max-key :total-sleep-minute total-sleep-minutes-by-guard-id))
        sleep-records-of-guard-who-slept-most (get sleep-records-by-guard-id guard-id-slept-most)
        sleep-minute-tracker-of-guard-who-slept-most (reflect-sleep-records
                                                      sleep-minute-frequency-tracker
                                                      sleep-records-of-guard-who-slept-most)
        minute-the-most-sleeping-guard-most-often-slept (.indexOf
                                                         sleep-minute-tracker-of-guard-who-slept-most
                                                         (apply max sleep-minute-tracker-of-guard-who-slept-most))]
    (* guard-id-slept-most
       minute-the-most-sleeping-guard-most-often-slept)))

(comment
 (solve input))
