(ns chapter4.exercises)

;; Extra Exercise - Implement map with reduce
(defn map-with-reduce
  [function collection]
  (reduce (fn [accumulator value]
            (conj accumulator (function value)))
          []
          collection)
  )

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

;; The functions we want to apply for the value
;; for each conversion
;; if it's the name, it's already OK, so do not change anything (identity)
;; if it's the glitter-index, then we need to convert from str to int (thus apply str->int)
(def function-to-apply {:name          identity
                        :glitter-index str->int})

(defn convert-value
  [which-key value]
  ((get function-to-apply which-key) value))

(defn parse-file
  "Convert a CSV into rows of columns"
  [text]
  (let [split-by-newline #(clojure.string/split % #"\n")
        split-by-comma #(clojure.string/split % #",")
        rows (split-by-newline text)
        rows-of-columns (map split-by-comma rows)]
    rows-of-columns))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (let [combine-name-and-index (fn [one-row] (map vector vamp-keys one-row))
        process-one-key (fn [result [vamp-key value]]
                          (assoc result vamp-key (convert-value vamp-key value)))
        process-one-row (fn [one-row] (reduce process-one-key {} (combine-name-and-index one-row)))]
    (map process-one-row rows)))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

;; Q1
(defn glitter-filter-names
  [minimum-glitter records]
  (let [entries (glitter-filter minimum-glitter records)]
    (map :name entries)))

;; Q2
(def suspects (mapify (parse-file (slurp filename))))

(defn append
  [list-of-suspects suspect]
  (conj list-of-suspects suspect))

;; Q3
(defn validate
  [keywords-to-validating-functions record]
  )