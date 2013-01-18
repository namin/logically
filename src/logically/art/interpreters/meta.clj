(ns logically.art.interpreters.meta
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

;; Section 17.2 Meta-Interpreters

;; Program 17.5 A meta-interpreter for pure Prolog
(defn solve-for [clause]
  (letfn [(solve0 [goal]
            (solve [goal]))
          (solve [goals]
            (conde
              [(== goals ())]
              [(fresh [g gs b]
                 (conso g gs goals)
                 (clause g b)
                 (solve b)
                 (solve gs))]))]
    solve0))

(defn solver-member-clause [a b]
  (conde
    [(fresh [x xs]
       (== a ['member x (lcons x xs)])
       (== b ()))]
    [(fresh [x y ys]
       (== a ['member x (lcons y ys)])
       (== b [['member x ys]]))]))

(def ex-solver-member (solve-for solver-member-clause))

;; Program 17.6 A meta-interpreter for pure Prolog in continutation style
(defn alt-solve-for [clause]
  (letfn [(solve0 [goal]
            (solve [goal] ()))
          (solve [gs1 gs2]
            (conde
              [(== gs1 ())
               (== gs2 ())]
              [(== gs1 ())
               (fresh [g gs]
                 (conso g gs gs2)
                 (solve [g] gs))]
              [(fresh [g gs gsr b]
                 (conso g gs gs1)
                 (clause g b)
                 (appendo gs gs2 gsr)
                 (solve b gsr))]))]
    solve0))

(def ex-alt-solver-member (alt-solve-for solver-member-clause))

;; Program 17.7 A tracer for Prolog
(defn solve-trace-for [clause]
  (letfn [(solve0 [goal]
            (solve [goal] 0))
          (solve [goals depth]
            (conde
              [(== goals ())]
              [(fresh [g gs b]
                 (conso g gs goals)
                 (clause g b)
                 (display g depth)
                 (solve b (inc depth))
                 (solve gs depth))]))
           (display [goal depth]
             (fn [a]
               (println (apply str (repeat depth "   ")) (walk* a goal))
               a))]
    solve0))

(def ex-trace-solver-member (solve-trace-for solver-member-clause))

;; Program 17.8 A meta-interpreter for building a proof tree
(defn solve-proof-for [clause]
  (letfn [(solve0 [goal tree]
            (solve [goal] tree))
          (solve [goals tree]
            (conde
              [(== goals ())
               (== tree ())]
              [(fresh [g gs ts b tb]
                 (conso g gs goals)
                 (clause g b)
                 (conde
                   [(!= ts ())
                    (conso [g '<-- tb] ts tree)]
                   [(== ts ())
                    (== [g '<-- tb] tree)])
                 (solve b tb)
                 (solve gs ts))]))]
    solve0))

(def ex-proof-solver-member (solve-proof-for solver-member-clause))

;; Program 17.9 A meta-interpreter for reasoning about uncertainty
(defn solve-cf-for [clause-cf chain-cf conj-cf]
  (letfn [(solve0 [goal certainty]
            (solve [goal] certainty))
          (solve [goals certainty]
            (conde
              [(== goals ())
               (== certainty 1)]
              [(fresh [g gs b cg cb cs]
                 (conso g gs goals)
                 (clause-cf g b cg)
                 (solve b cb)
                 (solve gs cs)
                 (project [cg cb cs]
                   (== certainty (conj-cf (chain-cf cg cb) cs))))]))]
    solve0))

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
       (== b [['parent parent child]])
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
  (letfn [(solve0 [goal certainty t]
            (solve [goal] certainty t))
          (solve [goals certainty t]
            (conde
              [(== goals ())
               (== certainty 1)]
              [(fresh [g gs b cg cb cs tb]
                 (conso g gs goals)
                 (clause-cf g b cg)
                 (project [t cg]
                   (== (> cg t) true)
                   (== tb (chain-t t cg)))
                 (solve b cb tb)
                 (solve gs cs t)
                 (project [cg cb cs]
                   (== certainty (conj-cf (chain-cf cg cb) cs))))]))]
    solve0))

(def ex-cft-solver-family (solve-cft-for solver-family-clause-cf * * /))
