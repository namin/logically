(ns logically.abs.db
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel fact p)

(defn db-get-fact [db f]
  (fn [a]
    (bind* (assoc-meta a :db [@db]) (fact f))))

(defn db-add-fact! [db f]
  (fresh [g]
         (copy-term f g)
         (fn [a]
           (do (swap! db #(-> % (pldb/db-fact fact (walk* a g))))
               a))))

(defn db-fresh-fact [db f]
  (let [old-db @db
        old-a  (atom nil)
        restore (fn []
                  (all
                   (fn [a]
                     (do
                       (reset! db old-db)
                       @old-a))))]
    (all
     (fn [a] (do (reset! old-a a) a))
     (conda
      [(db-get-fact db f) (restore) fail]
      [succeed])
     (restore))))

(defn db-retract-fact! [db f]
  (all
   (db-get-fact db f)
   (fn [a]
     (do (swap! db #(-> % (pldb/db-retraction fact (walk* a f))))
         a))))

(defn db-new []
  (atom (pldb/db)))
