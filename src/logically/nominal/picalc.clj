(ns logically.nominal.picalc
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
)

;; Inspired by The pi-calculus operational semantics example of alphaProlog
;; http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/

(defn symbolo [x]
  (predc x symbol? `symbol?))

(defn nomo [x]
  (predc x nom? `nom?))

(defn renameo [n x m y]
  (conde
    [(nomo m) (== n m) (== x y)]
    [(nomo m) (nom/hash n m) (== m y)]
    [(symbolo m) (== m y)]
    [(fresh [body bodyres]
       (nom/fresh [a]
         (== (nom/tie a body) m)
         (== (nom/tie a bodyres) y)
         (nom/hash a m)
         (nom/hash a x)
         (renameo n x body bodyres)))]
    [(== m '()) (== m y)]
    [(fresh [a d ares dres]
       (conso a d m)
       (conso ares dres y)
       (renameo n x a ares)
       (renameo n x d dres))]))

(declare b-stepo)

(defn stepo [p a q]
  (conde
    [(== `(~'tau ~q) p)
     (== a 'tau_a)]
    [(fresh [p1 p2]
       (== `(~'sum ~p1 ~p2) p)
       (stepo p1 a q))]
    [(fresh [p1 p2]
       (== `(~'sum ~p1 ~p2) p)
       (stepo p2 a q))]
    [(fresh [p1 p2 q1]
       (== `(~'par ~p1 ~p2) p)
       (== `(~'par ~q1 ~p2) q)
       (stepo p1 a q1))]
    [(fresh [p1 p2 q2]
       (== `(~'par ~p1 ~p2) p)
       (== `(~'par ~p1 ~q2) q)
       (stepo p2 a q2))]
    [(== `(~'par ~'ina ~q) p)
     (== a 'tau_a)]
    [(== `(~'par ~q ~'ina) p)
     (== a 'tau_a)]
    [(fresh [p1 p2 q1 q2 x]
       (nom/fresh [n]
         (== `(~'par ~p1 ~p2) p)
         (== a 'tau_a)
         (== `(~'res ~(nom/tie n `(~'par ~q1 ~q2))) q)
         (b-stepo p1 (nom/tie n `(~'in_a ~x ~n)) (nom/tie n q1))
         (b-stepo p2 (nom/tie n `(~'out_a ~x ~n)) (nom/tie n q2))))]
    [(fresh [p1 p2 q1 q2 x]
       (nom/fresh [n]
         (== `(~'par ~p1 ~p2) p)
         (== a 'tau_a)
         (== `(~'res ~(nom/tie n `(~'par ~q1 ~q2))) q)
         (b-stepo p1 (nom/tie n `(~'out_a ~x ~n)) (nom/tie n q1))
         (b-stepo p2 (nom/tie n `(~'in_a ~x ~n)) (nom/tie n q2))))]
    [(fresh [p0 q0]
       (nom/fresh [n]
         (== `(~'res ~(nom/tie n p0)) p)
         (== `(~'res ~(nom/tie n q0)) q)
         (stepo p0 a q0)
         (nom/hash n a)))]
    [(fresh [x y]
       (== `(~'out ~x ~y ~q) p)
       (== a `(~'out_a ~x ~y)))]
    [(fresh [p1 p2 q1 q2 r1 x y z]
       (nom/fresh [y]
         (== `(~'par ~p1 ~p2) p)
         (== a 'tau_a)
         (== `(~'par ~r1 ~q1) q)
         (b-stepo p1 (nom/tie y `(~'in_a ~x ~y)) (nom/tie y q1))
         (stepo p2 `(~'out_a ~x ~z) q2)
         (renameo y z q1 r1)))]
    [(fresh [p1 p2 q1 q2 r2 x z]
       (nom/fresh [y]
         (== `(~'par ~p1 ~p2) p)
         (== a 'tau_a)
         (== `(~'par ~q1 ~r2) q)
         (b-stepo p2 (nom/tie y `(~'in_a ~x ~y)) (nom/tie y q2))
         (stepo p1 `(~'out_a ~x ~z) q1)
         (renameo y z q2 r2)))]))

(defn b-stepo [p a q]
  (conde
    [(fresh [p1 p2]
       (== `(~'sum ~p1 ~p2) p)
       (b-stepo p1 a q))]
    [(fresh [p1 p2]
       (== `(~'sum ~p1 ~p2) p)
       (b-stepo p2 a q))]
    [(fresh [p1 p2 q1]
       (nom/fresh [m]
         (== `(~'par ~p1 ~p2) p)
         (== (nom/tie m `(~'par ~q1 ~p2)) q)
         (b-stepo p1 a (nom/tie m q1))))]
    [(fresh [p1 p2 q2]
       (nom/fresh [m]
         (== `(~'par ~p1 ~p2) p)
         (== (nom/tie m `(~'par ~p1 ~q2)) q)
         (b-stepo p2 a (nom/tie m q2))))]
    [(fresh [p0 x]
       (nom/fresh [y]
         (== `(~'in ~x ~(nom/tie y p0)) p)
         (== a (nom/tie y `(~'in_a ~x ~y)))
         (== (nom/tie y p0) q)))]
    [(fresh [p0 q0]
       (nom/fresh [n m]
         (== `(~'res ~(nom/tie n p0)) p)
         (== (nom/tie m `(~'res ~(nom/tie n q0))) q)
         (nom/hash n a)
         (b-stepo p0 a (nom/tie m q0))))]
    [(fresh [p0 q0 x]
       (nom/fresh [y n]
         (== `(~'res ~(nom/tie y p0)) p)
         (== (nom/tie y `(~'res ~(nom/tie n q0))) q)
         (== a (nom/tie y `(~'out_a ~x ~y)))
         (nom/hash y x)
         (stepo p0 `(~'out_a ~x ~y) q0)))]))
