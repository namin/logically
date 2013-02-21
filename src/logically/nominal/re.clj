(ns logically.nominal.re
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
)

;; Inspired by Regular expression and automata example of alphaProlog
;; http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/

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

(defn sep [x]
  (map #(symbol (str %)) x))

(def one 'one)
(def zero 'zero)
(defn plus [r1 r2] ['plus r1 r2])
(defn prod [r1 r2] ['prod r1 r2])
(defn star [r] ['star r])
(defn s [c] ['char c])
(def symb s)

(defn oneo [r] (== r one))
(defn zeroo [r] (== r zero))
(defn pluso [r1 r2 r] (== r (plus r1 r2)))
(defn prodo [r1 r2 r] (== r (prod r1 r2)))
(defn staro [r0 r] (== r (star r0)))
(defn symbo [c r] (all (!= c nil) (== r (symb c))))

(defn disjo [l1 l2]
  (conde
    [(== l1 [])]
    [(fresh [a b c l]
       (conso [a c b] l l1)
       (nom/hash a l2)
       (nom/hash b l2)
       (disjo l l2))]))

(defn equivo [r nfa]
  (nom/fresh [a b]
    (conde
      [(oneo r)
        (nfao a [[a nil b]] [b] nfa)]
      [(zeroo r)
        (nfao a [] [b] nfa)]
      [(fresh [c]
         (symbo c r)
         (nfao a [[a c b]] [b] nfa))]
      [(fresh [r1 r2 nfa1 nfa2 sa sb sc sd l1 l2 l3 lr]
         (pluso r1 r2 r)
         (appendo [[a nil sa] [a nil sc] [sb nil b] [sd nil b]] l3 lr)
         (nfao a lr [b] nfa)
         (nfao sa l1 [sb] nfa1)
         (nfao sc l2 [sd] nfa2)
         (equivo r1 nfa1)
         (equivo r2 nfa2)
         (disjo l1 l2)
         (appendo l1 l2 l3)
         (nom/hash a l3)
         (nom/hash b l3))]
      [(fresh [r1 r2 nfa1 nfa2 sa sb sc sd l1 l2 l3]
         (prodo r1 r2 r)
         (nfao sa (lcons [sb nil sc] l3) [sd] nfa)
         (nfao sa l1 [sb] nfa1)
         (nfao sc l2 [sd] nfa2)
         (equivo r1 nfa1)
         (equivo r2 nfa2)
         (disjo l1 l2)
         (appendo l1 l2 l3))]
      [(fresh [r0 nfa0 sa sb l0 lr]
         (staro r0 r)
         (appendo [[a nil b] [sb nil sa] [a nil sa] [sb nil b]] l0 lr)
         (nfao a lr [b] nfa)
         (nfao sa l0 [sb] nfa0)
         (nom/hash a l0)
         (nom/hash b l0)
         (equivo r0 nfa0))])))

(defn literalo [cs r]
  (conde
    [(== cs []) (oneo r)]
    [(fresh [c cs2 r2]
       (conso c cs2 cs)
       (prodo (symb c) r2 r)
       (literalo cs2 r2))]))
