(ns logically.art.interpreters.fsm_test
  (:use [logically.art.interpreters.fsm] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-ex-ndfa-1
  (is (= (run 3 [q]
           (ex-ndfa-1 q))
        '(() (a b) (a b a b))))
  (is (= (run* [q]
           (ex-ndfa-1 '(a b a b a b)))
        '(_0)))
  (is (= (run* [q]
           (ex-ndfa-1 '(a b a)))
        ()))
  (is (= (run* [q]
           (ex-ndfa-1 '(c)))
        ())))

(deftest test-ex-npda-palindromes
  (is (= (run 5 [q]
           (ex-npda-palindromes q))
        '((_0) (_0 _0) (_0 _1 _0) (_0 _1 _1 _0) (_0 _1 _2 _1 _0))))
  (is (= (run* [q]
           (ex-npda-palindromes '(a b a)))
        '(_0)))
  (is (= (run* [q]
           (ex-npda-palindromes '(a b a a)))
        ())))

