(ns part1)

(def input
  (slurp "../../../inputs/2018/Day2/input.txt"))

(defn run [opts]
  (let [{:keys [twos threes]} (->> input
                                   clojure.string/split-lines
                                   (map (comp (fn [frequencies->chars]
                                                {:two-detected   (if (frequencies->chars 2)
                                                                   1
                                                                   0)
                                                 :three-detected (if (frequencies->chars 3)
                                                                   1
                                                                   0)})
                                              #(group-by (fn [[_k v]] v) %)
                                              frequencies))
                                   (reduce
                                     (fn [{:keys [twos threes]}
                                          {:keys [two-detected
                                                  three-detected]}]
                                       {:twos   (+ twos two-detected)
                                        :threes (+ threes three-detected)})
                                     {:twos   0
                                      :threes 0}))]
    (prn (* twos threes))))

; run with
; cd year2018/day2
; clj -X part1/run
; => 7776
