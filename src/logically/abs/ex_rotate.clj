(ns logically.abs.ex_rotate
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn rotate-clause [head body]
  (conde
   [(fresh [xs ys as bs]
           (== head [:rotate xs ys])
           (== body [[:append as bs xs] [:append bs as ys]]))]
   [(fresh [ys]
           (== head [:append () ys ys])
           (== body []))]
   [(fresh [x xs ys zs]
           (== head [:append (lcons x xs) ys (lcons x zs)])
           (== body [[:append xs ys zs]]))]))
