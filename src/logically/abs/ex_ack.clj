(ns logically.abs.ex_ack
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn ack-clause [head body]
  (conde
   [(fresh [n]
           (== head [:ack 0 n [:s n]])
           (== body []))]
   [(fresh [m n]
           (== head [:ack [:s m] 0 n])
           (== body [[:ack m [:s 0] n]]))]
   [(fresh [m n o p]
           (== head [:ack [:s m] [:s n] o])
           (== body [[:ack [:s m] n p] [:ack m p o]]))]))
