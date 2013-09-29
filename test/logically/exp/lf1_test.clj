(ns logically.exp.lf1_test
  (:use [logically.exp.lf1] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-nat
  (is (= (run 3 [q] (nat-o q))
         '(z (s z) (s (s z))))))

(deftest test-plus
  (is (= (run 3 [q] (fresh [n1 n2 n3 o]
                           (plus-o n1 n2 n3 o)
                           (== q [n1 n2 n3 o])))
         '([z z z (plus-z z)]
           [z (s z) (s z) (plus-z (s z))]
           [z (s (s z)) (s (s z)) (plus-z (s (s z)))]))))

(deftest test-plus-s
  (is (= (run 2 [q] (fresh [n1 n2 n3 o oa]
                           (conso 'plus-s oa o)
                           (plus-o n1 n2 n3 o)
                           (== q [n1 n2 n3 o])))
         '([(s z) z (s z) (plus-s z z z (plus-z z))]
           [(s z) (s z) (s (s z)) (plus-s z (s z) (s z) (plus-z (s z)))]))))

(deftest test-sum-inc
  (is (= (run 2 [q] (fresh [n1 n2 n3 p1 p2 o]
                           (sum-inc-o n1 n2 n3 p1 p2 o)
                           (== q [n1 n2 n3 p1 p2 o])))
         '([z z z (plus-z z) (plus-z (s z)) (sum-inc-z z z z)]
           [z z (s z) (plus-z z) (plus-z (s z)) (sum-inc-z z z (s z))]))))
