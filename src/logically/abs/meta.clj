(ns logically.abs.meta
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn solve* [u== clause goals]
  (conde
   [(== goals ())]
   [(fresh [head others body]
           (conso head others goals)
           (clause u== head body)
           (solve* u== clause body)
           (solve* u== clause others))]))
