(defproject mdparser "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [instaparse "1.4.8"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.15"]]
  :source-paths ["src" "script"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :figwheel true
                :compiler {:main "mdparser.core"
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/mdparser.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true}}
               {:id "test"
                :source-paths ["src" "test"]
                :compiler {:output-to "resources/test/js/compiled/mdparser-tests.js"
                           :output-dir "resources/test/js/compiled/out"
                           :optimizations :whitespace
                           :pretty-print true}}
               {:id "min"
                :source-paths ["src"]
                :compiler {:main "mdparser.core"
                           :output-to "resources/public/js/compiled/mdparser.min.js"
                           :optimizations :advanced
                           :pretty-print  false
                           :elide-asserts true
                           :output-wrapper false
                           :parallel-build true
                           :checked-arrays :warn}
                :notify-command ["release/wrap_release.sh"]}]
              :test-commands {"test" ["phantomjs"
                                      "resources/test/test.js"
                                      "resources/test/test.html"]}}
  :hooks [leiningen.cljsbuild])
