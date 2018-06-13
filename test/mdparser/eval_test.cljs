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
             {"x" 2 "y" 1 "z" 2}))))
  (testing "tesing conditional operators"
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y == z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 0 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:y 1 :z 1})))
             {"x" 1 "y" 1 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1.0000001 :z 1})))
             {"x" 1 "y" 1.0000001 "z" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y != z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 1 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:y 1 :z 1})))
             {"x" 0 "y" 1 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1.0000001 :z 1})))
             {"x" 0 "y" 1.0000001 "z" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y < z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 1 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:y 1 :z 1})))
             {"x" 0 "y" 1 "z" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y > z;")))]
      (is (= (js->clj (f (clj->js {:y 2 :z 1})))
             {"x" 1 "y" 2 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1 :z 1})))
             {"x" 0 "y" 1 "z" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y <= z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 1 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:y 2 :z 1})))
             {"x" 0 "y" 2 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1 :z 1.0000001})))
             {"x" 1 "y" 1 "z" 1.0000001})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y >= z;")))]
      (is (= (js->clj (f (clj->js {:y 2 :z 1})))
             {"x" 1 "y" 2 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 0 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:y 1.0000001 :z 1})))
             {"x" 1 "y" 1.0000001 "z" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y && z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 1})))
             {"x" 1 "y" 1 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1 :z 0})))
             {"x" 0 "y" 1 "z" 0}))
      (is (= (js->clj (f (clj->js {:y 0 :z 0})))
             {"x" 0 "y" 0 "z" 0})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = y || z;")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 1})))
             {"x" 1 "y" 1 "z" 1}))
      (is (= (js->clj (f (clj->js {:y 1 :z 0})))
             {"x" 1 "y" 1 "z" 0}))
      (is (= (js->clj (f (clj->js {:y 0 :z 0})))
             {"x" 0 "y" 0 "z" 0})))))

(deftest test-functions
  (testing "testing basic functions"
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = floor(y);")))]
      (is (= (js->clj (f (clj->js {:y 1})))
             {"x" 1 "y" 1}))
      (is (= (js->clj (f (clj->js {:y 1.99999})))
             {"x" 1 "y" 1.99999})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = max(y, z);")))]
      (is (= (js->clj (f (clj->js {:y 1 :z 2})))
             {"x" 2 "y" 1  "z" 2}))
      (is (= (js->clj (f (clj->js {:y 2 :z 1})))
             {"x" 2 "y" 2  "z" 1}))
      (is (= (js->clj (f (clj->js {:y -2 :z 0})))
             {"x" 0 "y" -2 "z" 0})))))

(deftest test-assign-op
  (testing "assign ops"
    (let [f (create-fun (emitter/emit 2 (parser/parse "x += y;")))]
      (is (= (js->clj (f (clj->js {:x 0 :y 1})))
             {"x" 1 "y" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x -= y;")))]
      (is (= (js->clj (f (clj->js {:x 1 :y 1})))
             {"x" 0 "y" 1})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x *= y;")))]
      (is (= (js->clj (f (clj->js {:x 2 :y 3})))
             {"x" 6 "y" 3})))
    (let [f (create-fun (emitter/emit 2 (parser/parse "x %= y;")))]
      (is (= (js->clj (f (clj->js {:x 3 :y 2})))
             {"x" 1 "y" 2})))))

(deftest test-if
  (testing "simple if"
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = if(x,y,z);")))]
      (is (= (js->clj (f (clj->js {:x 0 :y 1 :z 2})))
             {"x" 2 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:x 0.1 :y 1 :z 2})))
             {"x" 1 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:x -0.1 :y 1 :z 2})))
             {"x" 1 "y" 1 "z" 2}))
      (is (= (js->clj (f (clj->js {:x 0.0000001 :y 1 :z 2})))
             {"x" 2 "y" 1 "z" 2}))))
  (testing "complex if"
    (let [f (create-fun (emitter/emit 2 (parser/parse "x = if(x - 3,y = w + c; y,z = w + k; w - 8);")))]
      (is (= (js->clj (f (clj->js {:x 4 :y 1 :z 2 :w 3 :c 4 :k 5})))
             {"x" 7 "y" 7 "z" 2 "w" 3 "c" 4 "k" 5}))
      (is (= (js->clj (f (clj->js {:x 3 :y 1 :z 2 :w 3 :c 4 :k 5})))
             {"x" -5 "y" 1 "z" 8 "w" 3 "c" 4 "k" 5})))))
