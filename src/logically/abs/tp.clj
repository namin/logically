(ns logically.abs.tp
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db]
        [logically.abs.lub]))

(defn prove [db goals]
  (conde
   [(fresh [b bs]
           (conso b bs goals)
           (db-get-fact db b)
           (prove db bs))]
   [(== goals ())]))

(defn operatoro [db c]
  (fresh [head body]
         (c head body)
         (prove db body)
         (set-union db head)))

(defn iterateo [db c]
  (conda
   [(all (operatoro db c) fail)]
   [(all (db-retract-fact! db :flag) (iterateo db c))]
   [succeed]))

(defn go [c q]
  (let [db (db-new)]
    (all
     (iterateo db c)
     (db-get-fact db q))))
