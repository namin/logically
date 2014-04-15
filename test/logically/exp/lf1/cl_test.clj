(ns logically.exp.lf1.cl_test
  (:use [logically.exp.lf1.cl])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(defn i [] (app (app (s) (k)) (k)))
(defn b [] (app (app (s) (app (k) (s))) (k)))
(defn c [] (app (app (s) (app (app (b) (b)) (s))) (app (k) (k))))
(defn q1 [] (app (app (s) (app (s) (app (s) (k)))) (app (app (s) (k)) (k))))
(defn q2 [] (app (app (s) (app (k) (app (s) (i)))) (k)))
(defn f0 [] (app (app (s) (app (s) (app (k) (app (s) (i))))) (i)))
(defn f [] (app (f0) (f0)))

(deftest test-cl
  (is (cl-ok))
  (is (= (run* [q] (fresh [a b c] (== q [c a b]) (cl-clauses c a b) (== a 'typ)))
        '([(exp) typ []]
          [(contracto _0 _1) typ [[_0 (exp)] [_1 (exp)]]]
          [(wo1 _0 _1) typ [[_0 (exp)] [_1 (exp)]]]
          [(wo _0 _1) typ [[_0 (exp)] [_1 (exp)]]])))
  (is (= (run 3 [q] (cl q (exp)))
         '((s) (k) (app (s) (s)))))
  (is (= (run 1 [q] (fresh [ev] (nom/fresh [alpha]
                                           (nom/hash alpha q)
                                           (cl ev (wo (app q alpha) (app alpha alpha))))))
         '((app (app (s) (app (s) (app (s) (k)))) (app (app (s) (k)) _0))))))
