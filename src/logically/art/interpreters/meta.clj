(ns logically.art.interpreters.meta
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

;; Section 17.2 Meta-Interpreters

;; Program 17.5 A meta-interpreter for pure Prolog (adapted)
(defn solve-for [clause]
  (letfn [(solve [goal]
            (conde
              [(== goal ())]
              [(fresh [g gs]
                 (conso g gs goal)
                 (solve g)
                 (solve gs))]
              [(fresh [b]
                 (clause goal b)
                 (solve b))]))]
    solve))

(defn solver-member-clause [a b]
  (conde
    [(fresh [x xs]
       (== a ['member x (lcons x xs)])
       (== b ()))]
    [(fresh [x y ys]
       (== a ['member x (lcons y ys)])
       (== b ['member x ys]))]))

(def ex-solver-member (solve-for solver-member-clause))

;; Program 17.6 A meta-interpreter for pure Prolog in continutation style
(defn alt-solve-for [clause]
  (letfn [(solve0 [goal]
            (solve goal ()))
          (solve [gs1 gs2]
            (conde
              [(== gs1 ())
               (== gs2 ())]
              [(== gs1 ())
               (fresh [g goals]
                 (conso g goals gs2)
                 (solve g goals))]
              [(fresh [a b goals1]
                 (conso a b gs1)
                 (appendo b gs2 goals1)
                 (solve a goals1))]
              [(fresh [b]
                 (clause gs1 b)
                 (solve b gs2))]))]
    solve0))

(def ex-alt-solver-member (alt-solve-for solver-member-clause))

;; Program 17.7 A tracer for Prolog
(defn solve-trace-for [clause]
  (letfn [(solve0 [goal]
            (solve goal 0))
          (solve [goal depth]
            (conde
              [(== goal ())]
              [(fresh [g gs]
                 (conso g gs goal)
                 (solve g depth)
                 (solve gs depth))]
              [(fresh [b]
                 (clause goal b)
                 (display goal depth)
                 (solve b (inc depth)))]))
           (display [goal depth]
             (fn [a]
               (println (apply str (repeat depth "   ")) (walk* a goal))
               a))]
    solve0))

(def ex-trace-solver-member (solve-trace-for solver-member-clause))

;; Program 17.8 A meta-interpreter for building a proof tree
(defn solve-proof-for [clause]
  (letfn [(solve [goal tree]
            (conde
              [(== goal ())
               (== tree ())]
              [(fresh [g gs t ts]
                 (conso g gs goal)
                 (conso t ts tree)
                 (solve g t)
                 (solve t ts))]
              [(fresh [b t]
                 (clause goal b)
                 (== [goal '<-- t] tree)
                 (solve b t))]))]
    solve))

(def ex-proof-solver-member (solve-proof-for solver-member-clause))

;; Program 17.9 A meta-interpreter for reasoning about uncertainty
(defn solve-cf-for [clause-cf chain-cf conj-cf]
  (letfn [(solve [goal certainty]
            (conde
              [(== goal ())
               (== certainty 1)]
              [(fresh [g gs c1 c2]
                 (conso g gs goal)
                 (solve g c1)
                 (solve gs c2)
                 (project [c1 c2] (== certainty (conj-cf c1 c2))))]
              [(fresh [b c1 c2]
                 (clause-cf goal b c1)
                 (solve b c2)
                 (project [c1 c2] (== certainty (chain-cf c1 c2))))]))]
    solve))

(defn make-clause-cf [clause]
  (fn [a b c]
    (all
      (== c 1)
      (clause a b))))

(def ex-cf-solver-member (solve-cf-for (make-clause-cf solver-member-clause) * min))

(defn solver-family-clause-cf [a b c]
  (conde
    [(== [a b c] '((parent adam chris) () 0.5))]
    [(== [a b c] '((parent allan chris) () 0.25))]
    [(== [a b c] '((parent aziz chris) () 0.25))]
    [(== [a b c] '((parent chris diana) () 1))]
    [(== [a b c] '((parent chris emily) () 0.1))]
    [(== [a b c] '((ancestor sam adam) () 0.7))]
    [(== [a b c] '((ancestor sam allan) () 0.8))]
    [(fresh [parent child grandparent]
       (== a ['grandparent grandparent child])
       (== b [['parent parent child] ['parent grandparent parent]])
       (== c 1))]
    [(fresh [parent child]
       (== a ['ancestor parent child])
       (== b ['parent parent child])
       (== c 1))]
    [(fresh [ancestor parent child]
       (== a ['ancestor ancestor child])
       (== b [['parent parent child] ['ancestor ancestor parent]])
       (== c 1))]))

(defn make-clause-from-clause-cf [clause-cf]
  (fn [a b]
    (fresh [c]
      (clause-cf a b c))))

(def ex-solver-family (solve-for (make-clause-from-clause-cf solver-family-clause-cf) ))

(def ex-cf-solver-family (solve-cf-for solver-family-clause-cf * *))

;; Program 17.10 Reasoning with uncertainty with threshold cutoff
(defn solve-cft-for [clause-cf chain-cf conj-cf chain-t]
  (letfn [(solve [goal certainty t]
            (conde
              [(== goal ())
               (== certainty 1)]
              [(fresh [g gs c1 c2]
                 (conso g gs goal)
                 (solve g c1 t)
                 (solve gs c2 t)
                 (project [c1 c2] (== certainty (conj-cf c1 c2))))]
              [(fresh [b c1 c2 t1]
                 (clause-cf goal b c1)
                 (project [t c1]
                   (== (> c1 t) true)
                   (== t1 (chain-t t c1)))
                 (solve b c2 t1)
                 (project [c1 c2] (== certainty (chain-cf c1 c2))))]))]
    solve))

(def ex-cft-solver-family (solve-cft-for solver-family-clause-cf * * /))
