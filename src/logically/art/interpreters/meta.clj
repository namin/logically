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

(defn solver-member [solve-for]
  (letfn [(clause [a b]
            (conde
              [(fresh [x xs]
                 (== a ['member x (lcons x xs)])
                 (== b ()))]
              [(fresh [x y ys]
                 (== a ['member x (lcons y ys)])
                 (== b ['member x ys]))]))]
    (solve-for clause)))

(def ex-solver-member (solver-member solve-for))

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

(def ex-alt-solver-member (solver-member alt-solve-for))

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

(def ex-trace-solver-member (solver-member solve-trace-for))

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

(def ex-proof-solver-member (solver-member solve-proof-for))

