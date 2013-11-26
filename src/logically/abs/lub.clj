(ns logically.abs.lub
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db]))

(defn set-union [db f]
  (conda [(db-get-fact db f) fail]
         [(db-add-fact! db f)
          (db-add-fact! db :flag)]))
