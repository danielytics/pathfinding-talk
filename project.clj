(defproject pathfinding "0.1.0-SNAPSHOT"
  :description "Code for Clojure Ireland talk on Dijkstra & A* pathfinding"
  :url "https://github.com/danielytics/pathfinding-talk"
  :license {:name "MIT License"
            :url "https://raw.githubusercontent.com/danielytics/pathfinding-talk/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.priority-map "0.0.7"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.22.0"]]
                   :dependencies [[pjstadig/humane-test-output "0.8.3"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]}})
