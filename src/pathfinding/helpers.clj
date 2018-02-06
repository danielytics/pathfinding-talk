(ns pathfinding.helpers)

;; Normally, Dijkstra is used because not all routes have equal cost.
;; But for our sample maze, all cells have the same cost since we are
;; only using Dijkstra as a stepping-stone towards A*
;; Therefore, simply use a constant value for all costs
(def CONSTANT-COST 1)


(defn make-offsets
  [x y]
  (for [[ox oy] [        [0 -1]
                 [-1 0]        [ 1 0]
                         [0  1]]]
    [(+ x ox) (+ y oy)]))

(defn find-neighbours
  [graph]
  (into
    {}
    (map
      (fn [[[x y] node]]
        (let [neighbours  (keep
                            (fn [pos]
                              (when (and (>= (first pos) 0)
                                         (>= (second pos) 0)
                                         (= (get-in graph [pos :type]) :empty))
                                pos))
                            (make-offsets x y))]
          [[x y] (assoc node
                        :neighbours neighbours
                        :costs (into {} (map (fn [n] [n CONSTANT-COST]) neighbours)))]))
      graph)))


(defn convert-to-graph
  "Takes a vector of strings and turns it into a vector of maps"
  [cols]
  (->> cols
    (mapcat
      (fn [y row]
        (map-indexed
          (fn [x cell]
            [[x y] {:raw  cell
                    :type (case cell
                            \#     :wall
                            \space :empty
                            \S     :empty
                            \E     :empty)}])
          row))
      (range))
    (into {})
    (find-neighbours)))

(defn parse-map
  "Parses a 2D vector of strings representation of a map into a graph of connected spaces"
  [input]
  (let [graph-map (convert-to-graph input)]
    (reduce
      (fn [result [[x y] {:keys [raw]}]]
        (case raw
         \S  (assoc result :start [x y])
         \E  (assoc result :end [x y])
         result))
      {:graph graph-map}
      graph-map)))


(defn unparse-graph
  [{:keys [graph]}]
  (->> (sort (map (comp vec reverse) (keys graph)))
       (map (fn [[y x]] [y (get {:wall \#
                                 :empty \space
                                 :path \.}
                                (get-in graph [[x y] :type]))]))
       (partition-by first)
       (mapv #(clojure.string/join (map second %)))))

(defn draw-graph
  [graph]
  (doseq [row (unparse-graph graph)]
    (println row)))


(defn mark-path
  [graph path]
  (let [path (set path)]
    (update
      graph
      :graph
      (fn [g]
        (->> g
          (map (fn [[k v]] [k (assoc v :type (if (get path k) :path (:type v)))]))
          (into {}))))))

(defn display-info!
  [visited solver-name graph]
  (println)
  (println solver-name "visited" (count visited) "nodes")
  (draw-graph (mark-path graph (keys visited)))
  visited)

