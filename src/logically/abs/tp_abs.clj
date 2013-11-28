(ns logically.abs.tp_abs
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db] :reload)
  (:use [logically.abs.lub] :reload)
  (:use [logically.abs.unif] :reload))

(defn prove [db flag u== goals]
  (conde
   [(fresh [b bs]
           (conso b bs goals)
           (conde
            [(db-get-fact db b)]
            [(fresh [b1 b2]
                    (== [:== b1 b2] b)
                    (uok u== b1 b2))])
           (prove db flag u== bs))]
   [(== goals ())]))

(defn operatoro [db flag u== c]
  (fresh [head body]
         (c head body)
         (prove db flag u== body)
         (lub db flag head)))

(defn iterateo [db flag u== c]
  (conda
   [(all (operatoro db flag u== c) fail)]
   [(all (flag-retract! flag) (iterateo db flag u== c))]
   [succeed]))

(defn go [u== c q]
  (let [db (db-new)
        flag (flag-new)]
    (all
     (iterateo db flag u== c)
     (db-get-fact db q))))
