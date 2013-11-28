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

(defn ack-norm-clause [u== head body]
  (fresh [a b c]
         (== head [:ack a b c])
         (conde
          [(fresh [n]
                  (u== a 0)
                  (u== b n)
                  (u== c [:s n])
                  (== body []))]
          [(fresh [m n d e f]
                  (u== a [:s m])
                  (u== b 0)
                  (u== c n)
                  (u== d m)
                  (u== e [:s 0])
                  (u== f n)
                  (== body [[:ack d e f]]))]
          [(fresh [d e f g h i m n o p]
                  (u== a [:s m])
                  (u== b [:s n])
                  (u== c p)
                  (u== d [:s m])
                  (u== e n)
                  (u== f o)
                  (u== g m)
                  (u== h o)
                  (u== i p)
                  (== body [[:ack d e f] [:ack g h i]]))])))


