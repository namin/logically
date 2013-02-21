(ns logically.nominal.re
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
)

(defn nfao [start delta accepting nfa]
  (== nfa ['nfa start delta accepting]))

(defn starto [nfa start]
  (fresh [delta accepting]
   (nfao start delta accepting nfa)))

(defn deltao [nfa delta]
  (fresh [start accepting]
    (nfao start delta accepting nfa)))

(defn acceptingo [nfa accepting]
  (fresh [start delta]
    (nfao start delta accepting nfa)))

(defn epsilono [nfa q1 q2]
  (conde
    [(== q1 q2)]
    [(!= q1 q2)
     (fresh [delta qi]
       (deltao nfa delta)
       (membero [q1 nil qi] delta)
       (epsilono nfa qi q2))]))

(defn nexto [nfa q1 s q2]
  (fresh [delta qi]
    (epsilono nfa q1 qi)
    (deltao nfa delta)
    (membero [qi s q2] delta)))

(defn nexto* [nfa q1 ss q2]
  (conde
    [(== q1 q2) (== ss [])]
    [(fresh [s l qi]
       (conso s l ss)
       (nexto nfa q1 s qi)
       (nexto* nfa qi l q2))]))

(defn traceo* [nfa q ss qs]
  (conde
    [(== ss []) (== qs [])]
    [(fresh [s l qi ql]
       (conso s l ss)
       (conso qi ql qs)
       (nexto nfa q s qi)
       (traceo* nfa qi l ql))]))

(defn accepto [nfa l]
  (fresh [q1 q2 q3 accepting]
    (starto nfa q1)
    (nexto* nfa q1 l q2)
    (epsilono nfa q2 q3)
    (acceptingo nfa accepting)
    (membero q3 accepting)))

(defn traceo [nfa l qs]
  (fresh [q]
    (starto nfa q)
    (traceo* nfa q l qs)))

