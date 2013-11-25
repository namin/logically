(ns logically.abs.tp
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn set-union [db f]
  (fresh [g]
         (copy-term f g)
         (conda [(db g) fail]
                [(fn [a] (do (fact db (walk* a f)) a))])))

(defn prove [db goals]
  (conde
   [(fresh [b bs]
           (conso b bs goals)
           (db b)
           (prove db bs))]
   [(== goals ())]))

(defn operatoro [db c]
  (fresh [head body]
         (c head body)
         (prove db body)
         (set-union db head)))

(defn iterateo [db c]
  (conda
   [(operatoro db c)
    (iterateo db c)]
   [succeed]))

(defn go [db c q]
  (all
   (iterateo db c)
   (db q)))
