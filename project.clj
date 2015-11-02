(defproject beatbot "0.1.0-SNAPSHOT"
  :description "An overtone/shadertoy playground for percussive synthesis"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [shadertone "0.2.5"]
                 [org.clojure/core.cache "0.6.4"]]
  :main beatbot.core
  :aot [beatbot.core]
  :repl-options {:init-ns beatbot.core})
