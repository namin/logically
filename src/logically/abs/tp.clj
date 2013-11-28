(ns logically.abs.tp
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db]
        [logically.abs.lub]))

(defn prove [db flag goals]
  (conde
   [(fresh [b bs]
           (conso b bs goals)
           (db-get-fact db b)
           (prove db flag bs))]
   [(== goals ())]))

(defn operatoro [db flag c]
  (fresh [head body]
         (c head body)
         (prove db flag body)
         (set-union db flag head)))

(defn iterateo [db flag c]
  (conda
   [(all (operatoro db flag c) fail)]
   [(all (flag-retract! flag) (iterateo db flag c))]
   [succeed]))

(defn go [c q]
  (let [db (db-new)
        flag (flag-new)]
    (all
     (iterateo db flag c)
     (db-get-fact db q))))
