(ns logically.abs.meta
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.unif] :reload))

(defn solve* [u== clause goals]
  (conde
   [(== goals ())]
   [(fresh [head others body]
           (conso head others goals)
           (conde
            [(fresh [a b]
                    (== head [:== a b])
                    (uok u== a b))]
            [(clause head body)
             (solve* u== clause body)])
           (solve* u== clause others))]))
