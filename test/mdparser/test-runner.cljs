(ns mdparser-tests.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [mdparser-tests.parser]))

(enable-console-print!)

(def success 0)

(defn ^:export run
  []
  (run-tests 'mdparser-tests.parser)
  success)
