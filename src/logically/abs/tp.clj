(ns logically.abs.tp
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel fact p)

(defn db-get-fact [db f]
  (fn [a]
    (bind* (assoc-meta a :db [@db]) (fact f))))

(defn db-add-fact! [db f]
  (fn [a]
    (do (swap! db #(-> % (pldb/db-fact fact (walk* a f))))
        a)))

(defn set-union [db f]
  (fresh [g]
         (copy-term f g)
         (conda [(db-get-fact db g) fail]
                [(db-add-fact! db f)])))

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
   [(operatoro db c)
    (iterateo db c)]
   [succeed]))

(defn go [c q]
  (let [db (atom (pldb/db))]
    (all
     (iterateo db c)
     (db-get-fact db q))))
