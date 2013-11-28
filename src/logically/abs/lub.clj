(ns logically.abs.lub
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db] :reload))

(defn set-union [db flag f]
  (all
   (conda [(db-get-fact db f) fail]
          [succeed])
   (db-add-fact! db f)
   (flag-raise! flag)))
