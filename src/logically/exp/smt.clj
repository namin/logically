(ns logically.exp.smt
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic :exclude [is] :as l])
  (:require [clojure.java.io :as io])
  (:use [clojure.java.shell :only [sh]]))

(def out-counter (atom 0))

(defn out-smt []
  (str "out" (swap! out-counter inc) ".smt"))

(defn spit-lines [smt-lines out-fn]
  (with-open [wrt (io/writer out-fn)]
    (doseq [x smt-lines]
      (.write wrt (clojure.string/replace (str x "\n") #"bv-" "#b")))))

(defn replace-back [s]
  (clojure.string/replace s #"#b" "bv-"))

(defn call-gen [smt-lines f]
  (let [out-fn (out-smt)]
    (spit-lines smt-lines out-fn)
    (let [r (f out-fn)]
      (replace-back (:out r)))))

(defn call-cvc4 [smt-lines]
  (call-gen (cons '(set-logic ALL_SUPPORTED) smt-lines)
            (fn [fn] (sh "cvc4" "-m" "--lang" "smt" fn))))

(defn call-z3 [smt-lines]
  (call-gen smt-lines
            (fn [fn] (sh "z3" fn))))

(def call-smt call-z3)

(defn read-sat [o]
  (let [i (.indexOf o "\n")
        m (.substring o 0 i)]
    (cond
      (= "sat" m) true
      (= "unsat" m) false
      :else (throw (AssertionError. (format "SMT solver answered '%s'" m))))))

(defn check-sat [smt-lines]
  (read-sat (call-smt (concat smt-lines '((check-sat) (exit))))))

(defn read-model [o]
  (let [i (.indexOf o "\n")]
    (if (= "sat" (.substring o 0 i))
      (let [s (.substring o (+ i 1))]
        (let [e (read-string s)]
          (map
           (fn [x] [(second x) (second (rest (rest (rest x))))])
           e)))
      false)))

(defn get-model [smt-lines]
  (read-model (call-smt (concat smt-lines '((check-sat) (get-model) (exit))))))

(defn neg-model [model]
  (let [cases
        (map (fn [xv]
               `(~'not (~'= ~(first xv) ~(second xv))))
             model)]
    (if (empty? cases)
      '(assert false)
      (cons 'assert (cons (cons 'or cases) nil)))))

(defn get-next-model [xs ms]
  (let [ys (concat xs (map neg-model ms))]
    (and (check-sat ys)
         (get-model ys))))

(defn add-model [m s]
  (fn [a]
    (if (empty? m)
      a
      (bind
       ((== (.valAt s (first (first m))) (second (first m)))
        a)
       (add-model (rest m) s)))))

(defn smt-purge-loop [ms smt-lines s a--]
  (let [m (get-next-model smt-lines ms)]
    (when m
      (mplus
       ((add-model m s) a--)
       (fn [] (smt-purge-loop (cons m ms) smt-lines s a--))))))

(defn smt-constraints [a rs]
  (let [cs (:cs a)
        cm (:cm cs)
        rs (concat (vals cm) rs)
        anys (filter #((-watched-stores %) ::smt-any) rs)
        ds (filter #((-watched-stores %) ::smt-decl) rs)
        rs (filter #((-watched-stores %) ::smt) rs)
        avs (set (filter (fn [x] (lvar? (walk a x))) (mapcat (fn [r] (-rands r)) anys)))
        dvs (set (filter (fn [x] (lvar? (walk a x))) (mapcat (fn [r] (-rands r)) ds)))
        vs (set (filter (fn [x] (lvar? (walk a x))) (mapcat (fn [r] (-rands r)) rs)))
        xs (into [] (clojure.set/union avs dvs vs))
        r (-reify* (with-meta empty-s (meta a)) xs)
        s (reduce (fn [m x] (assoc m (walk r x) x)) {} xs)
        rr (map (fn [x] (-reifyc x nil r a)) rs)
        xr (map (fn [x] (walk r x)) (clojure.set/difference vs (clojure.set/union avs dvs)))
        ranys (map (fn [x] (-reifyc x nil r a)) anys)
        smt-lines (concat
                   ranys
                   (map (fn [x] `(~'declare-const ~@(-reifyc x nil r a))) ds)
                   (map (fn [x] `(~'declare-const ~x ~'Int)) xr)
                   (map (fn [x] `(~'assert ~x)) rr))
        a-- (reduce (fn [a x] ((remcg x) a)) a rs)]
    [smt-lines s a--]))

(defn smt-purge [a]
  (let [[smt-lines s a--] (smt-constraints a [])]
    (when (check-sat smt-lines)
      (smt-purge-loop '() smt-lines s a--))))

(defn -smtc [p]
  (reify
    IConstraintStep
    (-step [this s]
      (reify
        clojure.lang.IFn
        (invoke [_ a]
          (let [[smt-lines _ _] (smt-constraints a [this])]
            (when (check-sat smt-lines)
              ((addcg this) a))))
        IRunnable
        (-runnable? [_]
          true)))
    IConstraintOp
    (-rator [_] `-smtc)
    (-rands [_] (filter lvar? (flatten p)))
    IReifiableConstraint
    (-reifyc [c v r s]
      (apply list (walk* r (walk* s p))))
    IConstraintWatchedStores
    (-watched-stores [this] #{::l/subst ::smt})))

(defn smtc [p]
  (cgoal (-smtc p)))

(defn -smt-decl [x t]
  (reify
    IConstraintStep
    (-step [this s]
      (reify
        clojure.lang.IFn
        (invoke [_ a]
          ((addcg this) a))
        IRunnable
        (-runnable? [_]
          false)))
    IConstraintOp
    (-rator [_] `-smt-decl)
    (-rands [_] [x])
    IReifiableConstraint
    (-reifyc [c v r s]
      `(~(walk* r (walk* s x)) ~t))
    IConstraintWatchedStores
    (-watched-stores [this] #{::l/subst ::smt-decl})))

(defn smt-decl [x t]
  (cgoal (-smt-decl x t)))

(defn -smt-any [xs p]
  (reify
    IConstraintStep
    (-step [this s]
      (reify
        clojure.lang.IFn
        (invoke [_ a]
          ((addcg this) a))
        IRunnable
        (-runnable? [_]
          false)))
    IConstraintOp
    (-rator [_] `-smt-any)
    (-rands [_] xs)
    IReifiableConstraint
    (-reifyc [c v r s]
      (apply list (walk* r (walk* s p))))
    IConstraintWatchedStores
    (-watched-stores [this] #{::l/subst ::smt-any})))

(defn smt-any [xs p]
  (cgoal (-smt-any xs p)))
