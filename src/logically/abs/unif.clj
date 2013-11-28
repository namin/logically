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
   [(nonlvaro y)
    (conde
     [(== y :even)]
     [(== y :odd)])
    (== x y)]
   [(lvaro y)
    (== x y)]))

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
   [(nonlvaro y)
    (conde
     [(== y :even)]
     [(== y :odd)]
     [(== y :zero)]
     [(== y :one)])
    (== x y)]
   [(lvaro y)
    (== x y)]))

(defn uok [u== x y]
  (u== x y))
