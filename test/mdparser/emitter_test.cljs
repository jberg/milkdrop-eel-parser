(ns mdparser-test.emitter
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mdparser.parser :as parser]
            [mdparser.emitter :as emitter]))

(deftest test-operators
  (testing "testing basic operators"
    (is (= (emitter/emit 2 (parser/parse "x = y + z;"))
           "a['x']=(a['y']+a['z']);"))
    (is (= (emitter/emit 2 (parser/parse "x = y - z;"))
           "a['x']=(a['y']-a['z']);"))
    (is (= (emitter/emit 2 (parser/parse "x = y * z;"))
           "a['x']=(a['y']*a['z']);"))
    (is (= (emitter/emit 2 (parser/parse "x = y / z;"))
           "a['x']=div(a['y'],a['z']);"))
    (is (= (emitter/emit 2 (parser/parse "x = y & z;"))
           "a['x']=bitand(a['y'],a['z']);"))
    (is (= (emitter/emit 2 (parser/parse "x = y | z;"))
           "a['x']=bitor(a['y'],a['z']);"))))

(deftest test-functions
  (testing "testing basic functions"
    (is (= (emitter/emit 1 (parser/parse "x = randint(y);"))
           "a['x']=rand(a['y']);"))
    (is (= (emitter/emit 2 (parser/parse "x = rand(y);"))
           "a['x']=rand(a['y']);"))
    (is (= (emitter/emit 2 (parser/parse "x = pow(y, z);"))
           "a['x']=pow(a['y'], a['z']);"))
    (is (= (emitter/emit 2 (parser/parse "x = floor(y);"))
           "a['x']=Math.floor(a['y']);"))
    (is (= (emitter/emit 2 (parser/parse "x = max(y, z);"))
           "a['x']=Math.max(a['y'], a['z']);"))))
