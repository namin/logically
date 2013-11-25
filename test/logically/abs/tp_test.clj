(ns logically.abs.tp_test
  (:use [logically.abs.tp] :reload)
  (:use [logically.abs.ex_path] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(defrel db p)
(deftest test-path
  (is (= (set (run* [q] (go db path-clause q)))
         #{[:edge :a :b]
           [:edge :b :a]
           [:edge :a :c]
           [:path :a :b]           
           [:path :b :a]
           [:path :a :c]
           [:path :a :a]
           [:path :b :b]           
           [:path :b :c]})))
