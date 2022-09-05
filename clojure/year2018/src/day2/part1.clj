(ns day2.part1)

(def input
  (slurp "../../inputs/2018/Day2/input.txt"))

(def group-chars-by-frequencies
  (comp #(group-by (fn [[_ch freq]] freq) %) frequencies))

(defn solve [input]
  (let [{:keys [twos threes]} (->> input
                                   clojure.string/split-lines
                                   (map group-chars-by-frequencies)
                                   (map (fn [frequencies->chars]
                                          {:two-detected   (if (frequencies->chars 2)
                                                             1
                                                             0)
                                           :three-detected (if (frequencies->chars 3)
                                                             1
                                                             0)}))
                                   (reduce
                                     (fn [{:keys [twos threes]}
                                          {:keys [two-detected
                                                  three-detected]}]
                                       {:twos   (+ twos two-detected)
                                        :threes (+ threes three-detected)})
                                     {:twos   0
                                      :threes 0}))]
    (* twos threes)))

(defn run [opts]
  (-> input
      solve
      prn))

; run with
; cd year2018/day2
; clj -X part1/run
; => 7776
