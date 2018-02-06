(ns pathfinding.core
 (:require
   [clojure.data.priority-map :refer [priority-map]]
   [pathfinding.helpers :refer [display-info!]]))

(defn walk-path
 [breadcrumbs start goal]
 (conj
   (->> start
        (iterate #(get breadcrumbs %))
        (take-while #(not= goal %)))
   goal))


(defn visit-nodes-bfs
  [graph start goal]
  (->> {:frontier [start]
        :came-from {start :start}}
       (iterate
         (fn [{:keys [frontier came-from]}]
           (when-let [current    (first frontier)]
             (when (not= current goal)
               (let [neighbours (remove #(contains? came-from %)
                                        (get-in graph [current :neighbours]))]
                 {:frontier  (concat (next frontier) neighbours)
                  :came-from (into came-from (map #(vector % current) neighbours))})))))
      (take-while identity)
      last
      :came-from))

(defn breadth-first-solver
  [{:keys [start end graph] :as g}]
  (-> graph
      (visit-nodes-bfs start end)
      (display-info! "BFS" g)
      (walk-path end start)))


(defn visit-nodes-dijkstra
  [graph start goal]
  (->> {:frontier (priority-map start 0)
        :cost-so-far {start 0}
        :came-from {start :start}}
       (iterate
         (fn [{:keys [frontier came-from cost-so-far]}]
           (when-let [current (first (peek frontier))]
             (when (not= current goal)
               (let [neighbours       (get-in graph [current :neighbours])
                     neighbour-costs  (->> neighbours
                                           (map (fn [neighbour]
                                                  (let [new-cost (+ (get cost-so-far current)
                                                                    (get-in graph [current :costs neighbour]))]
                                                    (when (or (not (contains? cost-so-far neighbour))
                                                              (< new-cost (get cost-so-far neighbour)))
                                                      [neighbour new-cost]))))
                                           (filter identity))]
                 {:frontier  (into (pop frontier) neighbour-costs)
                  :came-from (into came-from (map #(vector (first %) current) neighbour-costs))
                  :cost-so-far (into cost-so-far neighbour-costs)})))))
      (take-while identity)
      last
      :came-from))

(defn dijkstra-solver
  [{:keys [start end graph] :as g}]
  (-> graph
      (visit-nodes-dijkstra start end)
      (display-info! "Dijkstra" g)
      (walk-path end start)))


(defn heuristic
  [goal current]
  ; Manhattan distance on a square grid
  (let [[ax ay] goal
        [bx by] current]
    (+ (Math/abs (- ax bx))
       (Math/abs (- ay by)))))

(defn visit-nodes-a*
 [graph start goal]
 (->> {:frontier (priority-map start 0)
       :cost-so-far {start 0}
       :came-from {start :start}}
      (iterate
        (fn [{:keys [frontier came-from cost-so-far]}]
          (when-let [current (first (peek frontier))]
            (when (not= current goal)
              (let [neighbours       (get-in graph [current :neighbours])
                    neighbour-costs  (->> neighbours
                                          (map (fn [neighbour]
                                                 (let [new-cost (+ (get cost-so-far current)
                                                                   (get-in graph [current :costs neighbour]))]
                                                   (when (or (not (contains? cost-so-far neighbour))
                                                             (< new-cost (get cost-so-far neighbour)))
                                                     [neighbour new-cost]))))
                                          (filter identity))]
                {:frontier  (into (pop frontier)
                                  (map
                                    (fn [[node cost]] [node (+ cost (heuristic goal node))])
                                    neighbour-costs))
                 :came-from (into came-from (map #(vector (first %) current) neighbour-costs))
                 :cost-so-far (into cost-so-far neighbour-costs)})))))
     (take-while identity)
     last
     :came-from))

(defn a*-solver
  [{:keys [start end graph] :as g}]
  (-> graph
      (visit-nodes-a* start end)
      (display-info! "A*" g)
      (walk-path end start)))
