(ns pathfinding.core-test
 (:require [clojure.test :refer :all]
           [pathfinding.core :refer :all]
           [pathfinding.helpers :refer [parse-map unparse-graph draw-graph mark-path]]))

(defn solve
  [solver maze]
  (unparse-graph (mark-path maze (solver maze))))

(def maze ["###################"
           "#S                #"
           "#                 #"
           "###############  ##"
           "#                 #"
           "#    #            #"
           "#    #  # #########"
           "#    #  #    #    #"
           "#    #       #    #"
           "#    #########    #"
           "#                E#"
           "###################"])

(def solved-maze
          ["###################"
           "#...............  #"
           "#              .  #"
           "###############. ##"
           "#   ............  #"
           "#   .#            #"
           "#   .#  # #########"
           "#   .#  #    #    #"
           "#   .#       #    #"
           "#   .#########    #"
           "#   ..............#"
           "###################"])

(def maze-graph (parse-map maze))

(deftest breadth-first-test
  (testing "finding path through maze using breadth first search"
    (is (= solved-maze
           (solve breadth-first-solver maze-graph)))))

(deftest dijkstra-test
  (testing "finding path through maze using dijkstra shortest-path search"
    (is (= solved-maze
           (solve dijkstra-solver maze-graph)))))

(deftest a*-test
 (testing "finding path through maze using A* shortest-path search"
   (is (= solved-maze
          (solve a*-solver maze-graph)))))

