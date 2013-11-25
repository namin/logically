(ns logically.abs.ex_path
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn path-clause [head body]
  (conde
   [(fresh [x y]
           (== head [:path x y])
           (== body [[:edge x y]]))]
   [(fresh [x y z]
           (== head [:path x y])
           (== body [[:edge x z] [:path z y]]))]
   [(== body ())
    (conde
     [(== head [:edge :a :b])]
     [(== head [:edge :b :a])]
     [(== head [:edge :a :c])])]))

(defn path-symbol [x]
  (conde
   [(== x :a)]
   [(== x :b)]
   [(== x :c)]))
