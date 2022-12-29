(ns chapter3.exercises)
(require 'clojure.string)


;; Q1 - Use the data structures
(defn data-structures-tour
  []
  (let [chosen (rand-int 5)]
    (println "chosen " chosen)
    (cond
      (= chosen 0) (str "Placeholder" " " "value" " to showcase str")
      (= chosen 1) (vector 1 2 3 2 1)
      (= chosen 2) (list 3 4 5)
      (= chosen 3) (hash-map :name "Kelsier" :age 999)
      (= chosen 4) (hash-set "Kelsier" 1 2 3 2 1 "Kelsier")
      )))

;; Q2 - Function that takes a number and adds 100 to it
(defn add-100
  [number]
  (+ number 100))

;; Q3 - Similar to `inc-maker` but with subtraction
(defn dec-maker
  [number-to-subtract]
  #(- % number-to-subtract))

;; Q4 - Works like map but returns collection as a set
(defn mapset
  ;; We use a nested function to keep the implementation details private.
  ;; If we exposed both the 2-arity and the 3-arity functions, the user could
  ;; potentially call the mapset providing their own accumulator, which would
  ;; break the code (we want the accumulator to be a hash-set!).
  [function collection]
  (let [inner-function (fn
                         [function collection accumulator]
                         (let [[first-element & remaining] collection]
                           (if (nil? first-element)
                             accumulator
                             (recur function remaining (conj accumulator (function first-element))))))]
    (inner-function function collection #{})))