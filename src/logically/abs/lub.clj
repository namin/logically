(ns logically.abs.lub
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db]))

(defn set-union [db f]
  (fresh [g]
         (copy-term f g)
         (conda [(db-get-fact db g) fail]
                [(db-add-fact! db f)])))
