(ns logically.nominal.re_test
  (:use [logically.nominal.re] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(defn s [x]
  (map #(symbol (str %)) x))

(deftest test-nfa-1
  (is (= (run* [q]
           (fresh [fa]
             (nom/fresh [a b]
               (nfao a [[a 'a a] [a 'b b] [b 'a a] [b 'b b]] [b] fa)
               (accepto fa (s "abab")))))
        '(_0)))
  (is (= (run* [q]
           (fresh [fa]
             (nom/fresh [a b]
               (nfao a [[a 'a a] [a 'b b] [b 'a a] [b 'b b]] [b] fa)
               (accepto fa (s "ababa")))))
        '())))
