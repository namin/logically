(ns logically.exp.smt_test
  (:use [logically.exp.smt] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(deftest unsat-1
  (is (= '()
       (run* [q]
         (smtc `(~'= ~q 1))
         (smtc `(~'= ~q 2))
         (smtc `(~'= ~q 3))))))

(deftest sat-1
  (is (= '(1)
         (run* [q]
           (smtc `(~'= ~q 1))
           smt-purge))))

(deftest sat-2
  (is (= '(1 2)
         (run 2 [q]
           (smtc `(~'> ~q 0))
           smt-purge))))

(deftest sat-conde-1
  (is (= '(1 2)
         (run* [q]
           (conde
            [(smtc `(~'= ~q 1))]
            [(smtc `(~'= ~q 2))])
           smt-purge))))

(deftest mix-constraints
  (is (= '(1)
         (run* [q]
           (predc q number? nil)
           (conde
            [(smtc `(~'= ~q 1))]
            [(== 'hello q)])
           smt-purge))))

(defn faco [n out]
  (conde
   [(smtc `(~'= ~n 0))
    (smtc `(~'= ~out 1))]
   [(smtc `(~'> ~n 0))
    (fresh [n-1 r]
      (smtc `(~'= (~'- ~n 1) ~n-1))
      (smtc `(~'= (~'* ~n ~r) ~out))
      (faco n-1 r))]))

(deftest faco-7
  (is (= '([0 1] [1 1] [2 2] [3 6] [4 24] [5 120] [6 720])
       (run 7 [n out]
         (faco n out)
         smt-purge))))

(deftest faco-backwards-2
  (is (= '(2)
         (run* [q]
           (faco q 2)
           smt-purge))))

(deftest faco-backwards-6
  (is (= '(6)
         (run* [q]
           (faco q 720)
           smt-purge))))

