(ns mdparser-test.interp
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mdparser.parser :as parser]
            [mdparser.emitter :as emitter]
            [goog.object]))

(deftest test-operators
  (testing "testing basic operators"
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 1 :z 2}) (parser/parse "x = y + z;")) :env)
        (clj->js {:x 3 :y 1 :z 2})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 1 :z 2}) (parser/parse "x = y - z;")) :env)
        (clj->js {:x -1 :y 1 :z 2})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 1 :z 2}) (parser/parse "x = y * z;")) :env)
        (clj->js {:x 2 :y 1 :z 2})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 1 :z 2}) (parser/parse "x = y / z;")) :env)
        (clj->js {:x 0.5 :y 1 :z 2})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 3 :z 2}) (parser/parse "x = y % z;")) :env)
        (clj->js {:x 1 :y 3 :z 2})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 3 :z 2}) (parser/parse "x = y & z;")) :env)
        (clj->js {:x 2 :y 3 :z 2})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:y 3 :z 2}) (parser/parse "x = y | z;")) :env)
        (clj->js {:x 3 :y 3 :z 2}))))
  (testing "tesing conditional operators"
    (let [parse (parser/parse "x = y == z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 2}) parse) :env)
          (clj->js {:x 0 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1}) parse) :env)
          (clj->js {:x 1 :y 1 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1.0000001 :z 1}) parse) :env)
          (clj->js {:x 1 :y 1.0000001 :z 1}))))
    (let [parse (parser/parse "x = y != z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 2}) parse) :env)
          (clj->js {:x 1 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1}) parse) :env)
          (clj->js {:x 0 :y 1 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1.0000001 :z 1}) parse) :env)
          (clj->js {:x 0 :y 1.0000001 :z 1}))))
    (let [parse (parser/parse "x = y < z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 2}) parse) :env)
          (clj->js {:x 1 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1}) parse) :env)
          (clj->js {:x 0 :y 1 :z 1}))))
    (let [parse (parser/parse "x = y > z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 2 :z 1}) parse) :env)
          (clj->js {:x 1 :y 2 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1}) parse) :env)
          (clj->js {:x 0 :y 1 :z 1}))))
    (let [parse (parser/parse "x = y <= z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 2}) parse) :env)
          (clj->js {:x 1 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 2 :z 1}) parse) :env)
          (clj->js {:x 0 :y 2 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1.0000001}) parse) :env)
          (clj->js {:x 1 :y 1 :z 1.0000001}))))
    (let [parse (parser/parse "x = y >= z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 2 :z 1}) parse) :env)
          (clj->js {:x 1 :y 2 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 2}) parse) :env)
          (clj->js {:x 0 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1.0000001 :z 1}) parse) :env)
          (clj->js {:x 1 :y 1.0000001 :z 1}))))
    (let [parse (parser/parse "x = y && z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1}) parse) :env)
          (clj->js {:x 1 :y 1 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 0}) parse) :env)
          (clj->js {:x 0 :y 1 :z 0})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 0 :z 0}) parse) :env)
          (clj->js {:x 0 :y 0 :z 0}))))
    (let [parse (parser/parse "x = y || z;")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 1}) parse) :env)
          (clj->js {:x 1 :y 1 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 0}) parse) :env)
          (clj->js {:x 1 :y 1 :z 0})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 0 :z 0}) parse) :env)
          (clj->js {:x 0 :y 0 :z 0}))))))

(deftest test-functions
  (testing "testing basic functions"
    (let [parse (parser/parse "x = floor(y);")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1}) parse) :env)
          (clj->js {:x 1 :y 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1.99999}) parse) :env)
          (clj->js {:x 1 :y 1.99999}))))
    (let [parse (parser/parse "x = max(y, z);")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 1 :z 2}) parse) :env)
          (clj->js {:x 2 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y 2 :z 1}) parse) :env)
          (clj->js {:x 2 :y 2 :z 1})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:y -2 :z 0}) parse) :env)
          (clj->js {:x 0 :y -2 :z 0}))))))

(deftest test-assign-op
  (testing "assign ops"
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 0 :y 1}) (parser/parse "x = y;")) :env)
        (clj->js {:x 1 :y 1})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 0 :y 1}) (parser/parse "x += y;")) :env)
        (clj->js {:x 1 :y 1})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 1 :y 1}) (parser/parse "x -= y;")) :env)
        (clj->js {:x 0 :y 1})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 2 :y 3}) (parser/parse "x *= y;")) :env)
        (clj->js {:x 6 :y 3})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 3 :y 2}) (parser/parse "x %= y;")) :env)
        (clj->js {:x 1 :y 2}))))
  (testing "assign ops to buffer"
    (is
      ;; easier to compare with clojurescript for arrays
      (= (js->clj ((emitter/interp 2 (clj->js {:x 0 :y 1 :megabuf (clj->js [0 0 0 0 0])})
                                     (parser/parse "megabuf(x) = y;")) :env))
         {"x" 0 "y" 1 "megabuf" [1 0 0 0 0]}))
    (is
      (= (js->clj ((emitter/interp 2 (clj->js {:x 0 :y 1 :gmegabuf (clj->js [0 0 0 0 0])})
                                     (parser/parse "gmegabuf(x) = y;")) :env))
         {"x" 0 "y" 1 "gmegabuf" [1 0 0 0 0]}))))

(deftest test-if
  (testing "simple if"
    (let [parse (parser/parse "x = if(x,y,z);")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:x 0 :y 1 :z 2}) parse) :env)
          (clj->js {:x 2 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:x 0.1 :y 1 :z 2}) parse) :env)
          (clj->js {:x 1 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:x -0.1 :y 1 :z 2}) parse) :env)
          (clj->js {:x 1 :y 1 :z 2})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:x 0.0000001 :y 1 :z 2}) parse) :env)
          (clj->js {:x 2 :y 1 :z 2})))))
  (testing "complex if"
    (let [parse (parser/parse "x = if(x - 3,y = w + c; y,z = w + k; w - 8);")]
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:x 4 :y 1 :z 2 :w 3 :c 4 :k 5}) parse) :env)
          (clj->js {:x 7 :y 7 :z 2 :w 3 :c 4 :k 5})))
      (is
        (goog.object/equals
          ((emitter/interp 2 (clj->js {:x 3 :y 1 :z 2 :w 3 :c 4 :k 5}) parse) :env)
          (clj->js {:x -5 :y 1 :z 8 :w 3 :c 4 :k 5}))))))

(deftest test-loops
  (testing "loop"
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 0}) (parser/parse "loop(5,x += 1);")) :env)
        (clj->js {:x 5})))
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 5 :y 0 :z 0}) (parser/parse "loop(x,y += 1; z += y * y;);")) :env)
        (clj->js {:x 5 :y 5 :z 55}))))
  (testing "while"
    (is
      (goog.object/equals
        ((emitter/interp 2 (clj->js {:x 0 :y 4}) (parser/parse "while(exec2(x += 1; y = sqr(y);, y < 1000))")) :env)
        (clj->js {:x 3 :y 65536})))))

(deftest test-memcpy
  (testing
    (is
      (= (js->clj ((emitter/interp 2 (clj->js {:megabuf (js/Array. 1 2 3 4 5 0 0 0 0 0)})
                                     (parser/parse "memcpy(5, 0, 5)")) :env))
         {"megabuf" [1 2 3 4 5 1 2 3 4 5]}))))
