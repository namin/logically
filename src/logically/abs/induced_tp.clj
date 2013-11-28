(ns logically.abs.induced_tp
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db] :reload)
  (:use [logically.abs.lub] :reload))

(defn prove [db flag goals]
  (conde
   [(fresh [b bs]
           (conso b bs goals)
           (conda
            [(all (set-union db flag [:call b]) fail)]
            [(all (db-get-fact db [:ans b])
                  (prove db flag bs))]))]
   [(== goals ())]))

(defn operatoro [db flag c]
  (fresh [head body head]
         (c head body)
         (db-get-fact db [:call head])
         (prove db flag body)
         (set-union db flag [:ans head])))

(defn iterateo [db flag c]
  (conda
   [(all (operatoro db flag c) fail)]
   [(all (flag-retract! flag) (iterateo db flag c))]
   [succeed]))

(defn go [c g q]
  (let [db (db-new)
        flag (flag-new)]
    (all
     (db-add-fact! db [:call g])
     (iterateo db flag c)
     (db-get-fact db q))))

