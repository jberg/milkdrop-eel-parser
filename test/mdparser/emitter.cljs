(ns mdparser-tests.emitter
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mdparser.eqparser :as eqp]))

(deftest test-operators
  (testing "testing basic operators"
    (is (= (eqp/emit 2 (eqp/parse "x = y + z;"))
           "a['x']=(a['y']+a['z']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = y - z;"))
           "a['x']=(a['y']-a['z']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = y * z;"))
           "a['x']=(a['y']*a['z']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = y / z;"))
           "a['x']=div(a['y'],a['z']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = y & z;"))
           "a['x']=bitand(a['y'],a['z']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = y | z;"))
           "a['x']=bitor(a['y'],a['z']);"))))

(deftest test-functions
  (testing "testing basic functions"
    (is (= (eqp/emit 1 (eqp/parse "x = randint(y);"))
           "a['x']=rand(a['y']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = rand(y);"))
           "a['x']=rand(a['y']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = pow(y, z);"))
           "a['x']=pow(a['y'], a['z']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = floor(y);"))
           "a['x']=Math.floor(a['y']);"))
    (is (= (eqp/emit 2 (eqp/parse "x = max(y, z);"))
           "a['x']=Math.max(a['y'], a['z']);"))))
