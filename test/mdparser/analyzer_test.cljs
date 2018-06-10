(ns mdparser-test.analyzer
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mdparser.parser :as parser]
            [mdparser.emitter :as emitter]))

(deftest test-operators
  (testing "testing basic operators"
    (is (= ((emitter/analyze (parser/parse "x = y + z;") :per-frame) :user-vars)
           #{"x" "y" "z"}))
    (is (= ((emitter/analyze (parser/parse "x = y - z;") :per-frame) :user-vars)
           #{"x" "y" "z"}))
    (is (= ((emitter/analyze (parser/parse "x = y * z;") :per-frame) :user-vars)
           #{"x" "y" "z"}))
    (is (= ((emitter/analyze (parser/parse "x = y / z;") :per-frame) :user-vars)
           #{"x" "y" "z"}))
    (is (= ((emitter/analyze (parser/parse "x = y & z;") :per-frame) :user-vars)
           #{"x" "y" "z"}))
    (is (= ((emitter/analyze (parser/parse "x = y | z;") :per-frame) :user-vars)
           #{"x" "y" "z"}))))
