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

(defn db-new []
  (atom (pldb/db)))

(defn flag-new []
  (atom false))

(defn flag-retract! [flag]
  (all
   (fn [a] (bind* a (== @flag true)))
   (fn [a] (do (reset! flag false) a))))

(defn flag-raise! [flag]
  (fn [a] (do (reset! flag true) a)))

