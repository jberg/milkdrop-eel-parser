(ns mdparser-tests.parser
  (:require [cljs.test :refer-macros [deftest testing is]]
            [mdparser.eqparser :as eqp]))

(deftest test-operators
  (testing "testing basic operators"
    (is (= (eqp/parse "x = y + z;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:add-sub [:SYMBOL "y"] "+" [:SYMBOL "z"]]]]]))
    (is (= (eqp/parse "x = y - z;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:add-sub [:SYMBOL "y"] "-" [:SYMBOL "z"]]]]]))
    (is (= (eqp/parse "x = y * z;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:mult-div [:SYMBOL "y"] "*" [:SYMBOL "z"]]]]]))
    (is (= (eqp/parse "x = y / z;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:mult-div [:SYMBOL "y"] "/" [:SYMBOL "z"]]]]]))
    (is (= (eqp/parse "x = y & z;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:bitwise [:SYMBOL "y"] "&" [:SYMBOL "z"]]]]]))
    (is (= (eqp/parse "x = y | z;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:bitwise [:SYMBOL "y"] "|" [:SYMBOL "z"]]]]])))

  (testing "testing operator precedence"
    (is (= (eqp/parse "x = y + z | w;")
           [:PROGRAM
             [:STATEMENT
               [:ASSIGN [:SYMBOL "x"] "=" [:bitwise [:add-sub [:SYMBOL "y"] "+" [:SYMBOL "z"]] "|" [:SYMBOL "w"]]]]]))
    (is (= (eqp/parse "x = y | z + w;")
           [:PROGRAM
             [:STATEMENT
               [:ASSIGN [:SYMBOL "x"] "=" [:bitwise [:SYMBOL "y"] "|" [:add-sub [:SYMBOL "z"] "+" [:SYMBOL "w"]]]]]]))
    (is (= (eqp/parse "x = y * z + w;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:add-sub [:mult-div [:SYMBOL "y"] "*" [:SYMBOL "z"]] "+" [:SYMBOL "w"]]]]]))
    (is (= (eqp/parse "x = y + z * w;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:add-sub [:SYMBOL "y"] "+" [:mult-div [:SYMBOL "z"] "*" [:SYMBOL "w"]]]]]]))
    (is (= (eqp/parse "x = (y + z) * w;")
           [:PROGRAM
            [:STATEMENT
              [:ASSIGN [:SYMBOL "x"] "=" [:mult-div [:add-sub [:SYMBOL "y"] "+" [:SYMBOL "z"]] "*" [:SYMBOL "w"]]]]]))))
