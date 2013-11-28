(ns logically.abs.ex_plus
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn plus-norm-clause [head body]
  (fresh [a b c]
         (conde
          [(== head [:plus a b c])
           (conde
            [(fresh [x]
                    (== body [[:== a 0]
                              [:== b x]
                              [:== c x]]))]
            [(fresh [d e f x y z]
                    (== body [[:== a [:s x]]
                              [:== b y]
                              [:== c [:s z]]
                              [:== d x]
                              [:== e y]
                              [:== f z]
                              [:plus d e f]]))])]
          [(== head [:times a b c])
           (conde
            [(fresh [x]
                    (== body [[:== a 0]
                              [:== b x]
                              [:== c 0]]))]
            [(fresh [d e f g h i x y z t]
                    (== body [[:== a [:s x]]
                              [:== b y]
                              [:== c z]
                              [:== d x]
                              [:== e y]
                              [:== f t]
                              [:== g t]
                              [:== h y]
                              [:== i z]
                              [:times d e f] [:plus g h i]]))])])))


