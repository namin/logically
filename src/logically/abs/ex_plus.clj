(ns logically.abs.ex_plus
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn plus-norm-clause [u== head body]
  (fresh [a b c]
         (conde
          [(== head [:plus a b c])
           (conde
            [(fresh [x]
                    (u== a 0)
                    (u== b x)
                    (u== c x)
                    (== body []))]
            [(fresh [d e f x y z]
                    (u== a [:s x])
                    (u== b y)
                    (u== c [:s z])
                    (u== d x)
                    (u== e y)
                    (u== f z)
                    (== body [[:plus d e f]]))])]
          [(== head [:times a b c])
           (conde
            [(fresh [x]
                    (u== a 0)
                    (u== b x)
                    (u== c 0)
                    (== body []))]
            [(fresh [d e f g h i x y z t]
                    (u== a [:s x])
                    (u== b y)
                    (u== c z)
                    (u== d x)
                    (u== e y)
                    (u== f t)
                    (u== g t)
                    (u== h y)
                    (u== i z)
                    (== body [[:times d e f] [:plus g h i]]))])])))


