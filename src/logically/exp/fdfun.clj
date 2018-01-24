(ns logically.exp.fdfun
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:require [clojure.core.logic.fd :as fd]))

(defn fibo [n out]
  (fresh ()
    (fd/in n out (fd/interval 0 100))
    (conde
     [(fd/== n 0) (fd/== out 1)]
     [(fd/== n 1) (fd/== out 1)]
     [(fresh (n1 n2 out1 out2)
        (fd/in n1 n2 out1 out2 (fd/interval 0 100))
        (fd/> n 1)
        (fd/+ out1 out2 out)
        (fd/- n 1 n1)
        (fd/- n 2 n2)
        (fibo n1 out1)
        (fibo n2 out2))])))
