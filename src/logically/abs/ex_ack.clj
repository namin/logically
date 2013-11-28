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

(defn ack-norm-clause [head body]
  (fresh [a b c]
         (== head [:ack a b c])
         (conde
          [(fresh [n]
                  (== body [[:== a 0]
                            [:== b n]
                            [:== c [:s n]]]))]
          [(fresh [m n d e f]
                  (== body [[:== a [:s m]]
                            [:== b 0]
                            [:== c n]
                            [:== d m]
                            [:== e [:s 0]]
                            [:== f n]
                            [:ack d e f]]))]
          [(fresh [d e f g h i m n o p]
                  (== body [[:== a [:s m]]
                            [:== b [:s n]]
                            [:== c p]
                            [:== d [:s m]]
                            [:== e n]
                            [:== f o]
                            [:== g m]
                            [:== h o]
                            [:== i p]
                            [:ack d e f] [:ack g h i]]))])))


