(ns logically.abs.db
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn db-get-fact [db f]
  (fn [a] (bind* a (@db f))))

(defn db-add-fact! [db f]
  (fn [a]
    (let [fa (walk* a f)]
      (do (swap! db (fn [dbf]
                      (fn [x]
                        (conde
                         [(dbf x)]
                         [(fresh [g]
                                 (copy-term fa g)
                                 (== g x))]))))
          a))))

(defn db-new []
  (atom (fn [x] fail)))

(defn flag-new []
  (atom false))

(defn flag-retract! [flag]
  (all
   (fn [a] (bind* a (== @flag true)))
   (fn [a] (do (reset! flag false) a))))

(defn flag-raise! [flag]
  (fn [a] (do (reset! flag true) a)))

