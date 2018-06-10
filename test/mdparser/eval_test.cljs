(ns mdparser-test.eval
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mdparser.parser :as parser]
            [mdparser.emitter :as emitter]))

(defn create-fun
  [src]
  (js/Function. "a" (str src " return a;")))

(deftest test-operators
  (testing "testing basic operators"
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y + z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 3 "y" 1 "z" 2})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y - z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" -1 "y" 1 "z" 2})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y * z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 2 "y" 1 "z" 2})))))
