(ns logically.abs.unif
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn u==-concrete [x y]
  (== x y))

(defn u==-parity [x y]
  (conde
   [(fresh [s t]
           (nonlvaro y)
           (== y [:s t])
           (u==-parity s t)
           (conde
            [(== s :even) (== x :odd)]
            [(== s :odd) (== x :even)]))]
   [(nonlvaro y) (== y 0) (== x :even)]
   [(== x y)
    (conde
     [(nonlvaro y) (== y :even)]
     [(nonlvaro y) (== y :odd)]
     [(lvaro y)])]))

(defn u==-parity1 [x y]
  (conde
   [(fresh [s t]
           (nonlvaro y)
           (== y [:s t])
           (u==-parity1 s t)
           (conde
            [(== s :zero) (== x :one)]
            [(== s :one) (== x :even)]
            [(== s :even) (== x :odd)]
            [(== s :odd) (== x :even)]))]
   [(nonlvaro y) (== y 0) (== x :zero)]
   [(== x y)
    (conde
     [(nonlvaro y) (== y :even)]
     [(nonlvaro y) (== y :odd)]
     [(nonlvaro y) (== y :zero)]
     [(nonlvaro y) (== y :one)]
     [(lvaro y)])]))
