(ns logically.exp.smt_test
  (:use [logically.exp.smt] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(deftest unsat-1
  (is (= '()
       (run* [q]
         (smtc [q] `(~'= ~q 1))
         (smtc [q] `(~'= ~q 2))
         (smtc [q] `(~'= ~q 3))))))

(deftest sat-1
  (is (= '(1)
         (run* [q]
           (smtc [q] `(~'= ~q 1))
           smt-purge))))

(deftest sat-2
  (is (= '(1 2)
         (run 2 [q]
           (smtc [q] `(~'> ~q 0))
           smt-purge))))

(deftest sat-conde-1
  (is (= '(1 2)
         (run* [q]
           (conde
            [(smtc [q] `(~'= ~q 1))]
            [(smtc [q] `(~'= ~q 2))])
           smt-purge))))

(deftest mix-constraints
  (is (= '(1)
         (run* [q]
           (predc q number? nil)
           (conde
            [(smtc [q] `(~'= ~q 1))]
            [(== 'hello q)])
           smt-purge))))

