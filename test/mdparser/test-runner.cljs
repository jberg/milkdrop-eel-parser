(ns mdparser-test.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [mdparser-test.parser]
            [mdparser-test.emitter]))

(enable-console-print!)

(def success 0)

(defn ^:export run
  []
  (run-tests 'mdparser-test.parser
             'mdparser-test.emitter)
  success)
