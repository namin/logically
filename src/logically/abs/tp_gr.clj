(ns logically.abs.tp_gr
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

;; 3.1 of http://www.cs.bgu.ac.il/~mcodish/Tutorial/

(defn path-clause [head body]
  (conde
   [(fresh [x y]
           (== head [:path x y])
           (== body [[:edge x y]]))]
   [(fresh [x y z]
           (== head [:path x y])
           (== body [[:edge x z] [:path z y]]))]
   [(== body ())
    (conde
     [(== head [:edge :a :b])]
     [(== head [:edge :b :a])]
     [(== head [:edge :a :c])])]))

(defn path-symbol [x]
  (conde
   [(== x :a)]
   [(== x :b)]
   [(== x :c)]))

(defn prove [c s xs l]
  (conde
   [(fresh [x xr]
           (conso x xr xs)
           (membero x l)
           (prove c s xr l))]
   [(== xs ())]))

(defn ground-term [c s t]
  ;; TODO: extend to support function terms?
  (s t))

(defn ground-terms [c s ts]
  (conde
   [(== ts ())]
   [(fresh [t tr]
           (conso t tr ts)
           (ground-term c s t)
           (ground-terms c s tr))]))

(defn ground-instance [c s t]
  (fresh [p args]
         (nonlvaro t)
         (conso p args t)
         (ground-terms c s args)))


(defn tp [c s facts head]
  (fresh [body]
         (c head body)
         (prove c s body facts)
         (ground-instance c s head)))

(defn deriveo [c s facts out]
  (fn [a]
    (let [facts (walk* a facts)]
      (bind* a
       (== out
           (run* [q]
                 (tp c s facts q)))))))

(defn extendo [xs ys ext acc diff]
  (conde
   [(fresh [x xr]
           (conso x xr xs)
           (conda
            [(membero x ys)
             (extendo xr ys ext acc diff)]
            [(extendo xr (lcons x ys) ext (lcons x acc) diff)]))]
   [(== xs ())
    (== ys ext)
    (== acc diff)]))

(defn iterate-tp [c s facts diff result]
  (conda
   [(== diff ())
    (== facts result)]
   [(fresh [facts1 diff1 ext]
           (deriveo c s facts facts1)
           (extendo facts1 facts ext () diff1)
           (iterate-tp c s ext diff1 result))]))




