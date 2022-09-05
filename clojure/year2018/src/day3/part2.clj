(ns day3.part2
  (:require [day3.part1 :as part1]))

(def claims part1/claims)

(defn claim-overlap? [claim1 claim2]
  (let [[_ [claim1-start-x claim1-start-y] _] claim1
        [_ [claim2-start-x claim2-start-y] _] claim2
        [claim-left claim-right] (if (< claim1-start-x claim2-start-x)
                                   [claim1 claim2]
                                   [claim2 claim1])
        [_ [claim-left-start-x claim-left-start-y] [claim-left-width claim-left-height]] claim-left
        [_ [claim-right-start-x claim-right-start-y] [claim-right-width claim-right-height]] claim-right]
    (or (and (< claim-right-start-x (+ claim-left-start-x claim-left-width))

             (or (and (<= claim-left-start-y claim-right-start-y)
                      (< claim-right-start-y (+ claim-left-start-y claim-left-height)))

                 (and (> claim-left-start-y claim-right-start-y)
                      (> (+ claim-right-start-y claim-right-height) claim-left-start-y)))))))

(claim-overlap? ["1" [1 3] [4 4]] ["2" [3 1] [4 4]])
(claim-overlap? ["1" [1 3] [4 4]] ["3" [5 5] [2 2]])
(claim-overlap? ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])

(def get-claim-id first)

(def non-overlapping-claims
  (filter (fn [claim1]
            (every? (fn [claim2]
                      (or (not (claim-overlap? claim1 claim2))
                          (= (get-claim-id claim1) (get-claim-id claim2))))
                    claims))
          claims))

non-overlapping-claims
