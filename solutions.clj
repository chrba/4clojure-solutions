
;106. Number Maze
;Given a pair of numbers, the start and end point, find a path between the two using 
;only three possible operations: double halve (odd numbers cannot be halved) add 2
;Find the shortest path through the "maze". Because there are multiple shortest paths, 
;you must return the length of the shortest path, not the path itself.
(fn number-maze
  [start end]
  (letfn [(nm [nodes depth]
            (if (some #{end} nodes) depth
                (let [op (fn [i]
                           [(* 2 i) (+ 2 i) (if (even? i) (/ i 2) [])])
                      next (flatten (map op nodes))]
                  (recur next (inc depth)))))]
    (nm [start] 1)))





;111. Crossword puzzle
;Write a function that takes a string and a partially-filled crossword puzzle board, and 
;determines if the input string can be legally placed onto the board. 
;
;The crossword puzzle board consists of a collection of partially-filled rows. Empty spaces are 
;denoted with an underscore (_), unusable spaces are denoted with a hash symbol (#), and pre-filled spaces 
;have a character in place; the whitespace characters are for legibility and; should be ignored. 

;For a word to be legally placed on the board: 
;- It may use empty spaces (underscores) 
;- It may use but must not conflict with any pre-filled characters. 
;- It must not use any unusable spaces (hashes). 
;- There must be no empty spaces (underscores) or extra characters before or after the word 
;  (the word may be bound by unusable spaces though). 
;- Characters are not case-sensitive. 
;- Words may be placed vertically (proceeding top-down only), or horizontally 
;  (proceeding left-right only).
(fn cross-puzzle [word puzzle]
  (letfn [(fits? [x] (if (not= (count word) (count x) ) false
                            (->> x
                                 (interleave word)
                                 (partition 2)
                                 (map #(cond
                                        (= (second %) \_) true
                                        (= (first %) (second %)) true))
                                 (every? identity))))
          (rotate [board] (apply map str board))
          (clear-space [col] (map #(apply str (remove #{\space} %)) col))
          (normalize [board] (flatten (map #(clojure.string/split % #"#") (clear-space board))))]
    (boolean (or (some fits? (normalize puzzle))
        (some fits? (normalize (rotate puzzle)))))))




;112. Sequs Horribilis
;Create a function which takes an integer and a nested collection of integers 
;as arguments. Analyze the elements of the input collection and return a sequence 
;which maintains the nested structure, and which includes all elements starting
; from the head whose sum is less than or equal to the input integer.
(fn sequs-horribilis
  [n coll]
  (letfn [(traverse [c sum max]
            (cond
             (nil? (seq c)) [nil sum]
             (coll? (first c))
                 (let [[r sum] (traverse (first c) sum max)]
                     (vector (list r) sum))
             (<= (+ (first c) sum) max)
                 (let [[r sum] (traverse (next c) (+ (first c) sum) max)]
                   (vector (cons (first c) r) sum))
             :else [nil sum]
             ))]
    (vec (first (traverse coll 0 n)))))


;113. Making Data Dance
;Write a function that takes a variable number of integer arguments. If the output 
;is coerced into a string, it should r;eturn a comma (and space) separated list of
; the inputs sorted smallest to largest. If the output is coerced into a sequence,
; it should return a seq of unique input elements in the same order as they were entered.
 (fn data-dance
  [ & coll]
  (reify clojure.lang.ISeq
    (toString [_] (apply str (interpose ", "(sort coll))))
    (seq [_]  (seq (distinct coll)))))



(defn prime-gen
  ([] (prime-gen 2 {}))
  ([i m]
     (cond
      (nil? (m i)) (cons i
                    (lazy-seq (prime-gen (inc i) (assoc m (* i i) i))))
      :else
        (let [p (m i)]
          (lazy-seq
           (prime-gen (inc i)
                      (assoc m
                        (some #(when (nil? (m %)) %)  (iterate (partial + p) i))
                        p)))))))
 
