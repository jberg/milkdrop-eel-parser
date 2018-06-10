(ns mdparser-test.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [mdparser-test.parser]
            [mdparser-test.emitter]
            [mdparser-test.analyzer]
            [mdparser-test.eval]))

(enable-console-print!)

(def success 0)

(defn ^:export run
  []
  (run-tests 'mdparser-test.parser
             'mdparser-test.emitter
             'mdparser-test.analyzer
             'mdparser-test.eval)
  success)
