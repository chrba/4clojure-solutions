
;#106

;Given a pair of numbers, the start and end point, find a path between the two using only three possible operations:
;double
;halve (odd numbers cannot be halved)
;add 2
;Find the shortest path through the "maze". Because there are multiple shortest paths, you must return the length of the shortest path, not the path itself.

(fn number-maze
  [start end]
  (letfn [(nm [nodes depth]
            (if (some #{end} nodes) depth
                (let [op (fn [i]
                           [(* 2 i) (+ 2 i) (if (even? i) (/ i 2) [])])
                      next (flatten (map op nodes))]
                  (recur next (inc depth)))))]
    (nm [start] 1)))
