(ns logically.exp.fexpr
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
)

;; Inspired by Mitch Wand's Paper
;; The Theory of Fexprs is Trivial

(defn nomo [x]
  (predc x nom? nil))

(defn termo [m]
  (conde
   [(nomo m)]
   [(fresh [m1]
      (nom/fresh [x]
        (== m ['lambda (nom/tie x m1)]))
      (termo m1))]
   [(fresh [m1 m2]
      (== m [m1 m2])
      (termo m1)
      (termo m2))]
   [(fresh [m1]
      (== m ['fexpr m1])
      (termo m1))]
   [(fresh [m1]
      (== m ['eval m1])
      (termo m1))]))

(defn semo [m v]
  (fresh [z]
    (nom/fresh [a b c d e]
      (== v ['lambda (nom/tie a ['lambda (nom/tie b ['lambda (nom/tie c ['lambda (nom/tie d ['lambda (nom/tie e z)])])])])])
      (conde
       [(nomo m)
        (== z [a m])]
       [(fresh [m1 m2 v1 v2]
          (== m [m1 m2])
          (== z [[b v1] v2])
          (semo m1 v1)
          (semo m2 v2))]
       [(fresh [m1 v1]
          (nom/fresh [x]
            (== m ['lambda (nom/tie x m1)])
            (== z [c ['lambda (nom/tie x v1)]]))
          (semo m1 v1))]
       [(fresh [m1 v1]
          (== m ['fexpr m1])
          (== z [d v1])
          (semo m1 v1))]
       [(fresh [m1 v1]
          (== m ['eval m1])
          (== z [e v1])
          (semo m1 v1))]))))

(defn valueo [m]
  (conde
   [(fresh [m1]
      (nom/fresh [x]
        (== m ['lambda (nom/tie x m1)])))]
   [(fresh [m1]
      (== m ['fexpr m1])
      (valueo m1))]))

(defn substo [m new a out]
  (conde
   [(nomo m) (== m a) (== new out)]
   [(nomo m) (!= m a) (== m out)]
   [(fresh [m1 o1]
      (nom/fresh [y]
        (== m ['lambda (nom/tie y m1)])
        (== out ['lambda (nom/tie y o1)])
        (nom/hash y a)
        (nom/hash y new)
        (substo m1 new a o1)))]
   [(fresh [m1 m2 o1 o2]
      (== m [m1 m2])
      (== out [o1 o2])
      (substo m1 a new o1)
      (substo m2 a new o2))]
   [(fresh [m1 o1]
      (== m ['fexpr m1])
      (== out ['fexpr m1])
      (substo m1 new a o1))]
   [(fresh [m1 o1]
      (== m ['eval m1])
      (== out ['eval o1])
      (substo m1 new a o1))]))

(defn reduceo [m-in m-out]
  (conde
   [(fresh [m v]
      (nom/fresh [x]
        (== m-in [['lambda (nom/tie x m)] v])
        (valueo v)
        (substo m v x m-out)))]
   [(fresh [m v mv]
      (== m-in [['fexpr v] m])
      (valueo v)
      (semo m mv)
      (== m-out [v mv]))]
   [(fresh [m mv]
      (== m-in ['eval mv])
      (valueo mv)
      (semo m mv)
      (== m-out m))]
   [(fresh [m1 m2 m1p]
      (== m-in [m1 m2])
      (== m-in [m1p m2])
      (reduceo m1 m1p))]
   [(fresh [m1 m2 m2p]
      (nom/fresh [x]
        (== m-in [['lambda (nom/tie x m1)] m2])
        (== m-out [['lambda (nom/tie x m1)] m2p]))
      (reduceo m2 m2p))]
   [(fresh [m1 m1p]
      (== m-in ['fexpr m1])
      (== m-out ['fexpr m1p])
      (reduceo m1 m1p))]
   [(fresh [m1 m1p]
      (== m-in ['eval m1])
      (== m-out ['eval m1p])
      (reduceo m1 m1p))]))

(defn reduceo* [m v]
  (conde
    [(valueo m) (== m v)]
    [(fresh [mi]
       (reduceo m mi)
       (reduceo* mi v))]))
