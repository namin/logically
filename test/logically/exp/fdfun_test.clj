(ns logically.exp.fdfun_test
  (:use [logically.exp.fdfun] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:require [clojure.core.logic.fd :as fd])
  (:use [clojure.test]))

(deftest fibo-star
  (is (= (run* [n out]
           (fd/<= n 10)
           (fibo n out))
         '([0 1]
           [1 1]
           [2 2]
           [3 3]
           [4 5]
           [5 8]
           [6 13]
           [7 21]
           [8 34]
           [9 55]
           [10 89]))))

(deftest fibo-backwards-base
  (is (= (run* [n out]
           (== out 1)
           (fibo n out))
         '([0 1] [1 1]))))

(deftest fibo-backwards-rec
  (is (= (run* [n out]
           (== out 8)
           (fibo n out))
         '([5 8]))))
