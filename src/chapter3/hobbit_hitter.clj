(ns chapter3.hobbit-hitter)
(require 'clojure.string)

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}
                             ])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

;; Without reduce
(defn symmetrize-body-parts
  "Expects a sequence of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

;; With reduce
(defn better-symmetrize-body-parts
  [asym-body-parts]
  (reduce
    (fn [final-body-parts part] (into final-body-parts (set [part (matching-part part)])))
    []
    asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [all-body-parts (better-symmetrize-body-parts asym-body-parts)
        all-hit-points (reduce + (map :size all-body-parts))
        actual-hit (rand all-hit-points)]
    (loop [[current-part & remaining-parts] all-body-parts
           accumulated-hit-points (:size current-part)]
      (if (> accumulated-hit-points actual-hit)
        current-part
        (recur remaining-parts (+ accumulated-hit-points (:size current-part)))))))
