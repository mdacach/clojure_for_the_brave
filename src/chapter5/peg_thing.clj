(ns chapter5.peg-thing)
(require 'clojure.string)

(declare prompt-move)

(defn my-comp
  [& fns]
  (fn [& args]
    (let [ordered-fns (reverse fns)
          first-result (apply (first ordered-fns) args)
          remaining-fns (rest ordered-fns)]
      (reduce (fn [result next-fn] (next-fn result))
              first-result remaining-fns))))

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(<= % n) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  pos 2 and 3 in row 2, etc"
  [position]
  (first (drop-while #(< (row-tri %) position) (drop 1 (range)))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos1 jumped-over pos2]
  (let [connect-p2-to-p1 (fn [new-board [start end]]
                           (assoc-in new-board [start :connections end] jumped-over))]
    (if (and (<= pos2 max-pos) (<= pos1 max-pos))
      (reduce connect-p2-to-p1 board [[pos1 pos2] [pos2 pos1]])
      board)))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (+ pos 1)
        destination (+ pos 2)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ pos row)                                ;; Notice that this pattern always holds
        destination (+ row 1 neighbor)]                     ;; And the leap from the neighbor is row + 1
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ pos row 1)
        destination (+ row 2 neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-fn] (connection-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take the peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board start-pos]
  (into {}
        (filter (fn [[end-pos jumped-over]]
                  (and (pegged? board jumped-over) (not (pegged? board end-pos))))
                (get-in board [start-pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  [board p1 p2]
  (if-let [jumped-over (valid-move? board p1 p2)]
    (remove-peg (move-peg board p1 p2) jumped-over)
    nil))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (let [pegged-positions (map first (filter #(get (second %) :pegged) board))
        has-valid-move (comp not-empty (partial valid-moves board))
        ]
    (some has-valid-move pegged-positions)))

(def alpha-start 97)                                        ;; ASCII for letter 'a'
(def alpha-end 123)                                         ;; one after ASCII for letter 'z'
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

;; Note this wasn't in the book
;; I found it on: https://github.com/flyingmachine/pegthing/blob/master/src/pegthing/core.clj
(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-pos
  [board pos]
  (let [corresponding-letter (nth letters (dec pos))        ;; 0-indexed
        corresponding-marker (if (pegged? board pos) (colorize "O" :blue) (colorize "-" :red))]
    (str corresponding-letter corresponding-marker)))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (let [previous-row (dec row-num)
        last-value-in-previous-row (or (row-tri previous-row) 0)
        last-value-in-this-row (row-tri row-num)]
    (range (inc last-value-in-previous-row) (inc last-value-in-this-row))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (let [character (first letter)
        as-num (int character)]
    (inc (- as-num alpha-start))))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn characters-as-strings
  "Given a string, return a collection consisting of each individual character"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer/parseInt (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game Over! You had " remaining-pegs " pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= input "y")
        (prompt-rows)
        (do (println "Bye!")
            (System/exit 0))))))

(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (println input)
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(prompt-rows)