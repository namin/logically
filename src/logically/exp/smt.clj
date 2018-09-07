(ns logically.exp.prop
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic :exclude [is] :as l])
  (:require [clojure.java.io :as io])
  (:use [clojure.java.shell :only [sh]]))

(defn spit-lines [smt-lines]
  (with-open [wrt (io/writer "out.smt")]
  (doseq [x smt-lines]
    (.write wrt (str x "\n")))))

(defn call-cvc4 [smt-lines]
  (do
    (spit-lines (cons '(set-logic ALL-SUPPORTED) smt-lines))
    (let [r (sh "cvc4" "-m" "--lang" "smt" "out.smt")]
      (:out r))))

(defn read-sat [o]
  (let [i (.indexOf o "\n")]
    (= "sat" (.substring o 0 i))))

(defn check-sat [smt-lines]
  (read-sat (call-cvc4 (concat smt-lines '((check-sat) (exit))))))

(defn read-model [o]
  (let [i (.indexOf o "\n")]
    (if (= "sat" (.substring o 0 i))
      (let [s (.substring o (+ i 1))]
        (let [e (read-string s)]
          (map
           (fn [x] [(second x) (second (rest (rest (rest x))))])
           (rest e))))
      false)))

(defn get-model [smt-lines]
  (read-model (call-cvc4 (concat smt-lines '((check-sat) (get-model) (exit))))))

(defn neg-model [model]
  (cons 'assert (cons (cons 'or
  (map (fn [xv]
         `(~'not (~'= ~(first xv) ~(second xv))))
       model)) nil)))

(defn get-next-model [xs ms]
  (let [ys (concat xs (map neg-model ms))]
    (and (check-sat ys)
         (get-model ys))))

(defn -smtc [xs p]
  (reify
    IConstraintStep
    (-step [this s]
      (reify
        clojure.lang.IFn
        (invoke [_ a]
          (let [a ((addcg this) a)
                cs (:cs a)
                cm (:cm cs)
                rs (vals cm)
                xs (into [] (set (mapcat (fn [r] (-rands r)) rs)))
                r (-reify* (with-meta empty-s (meta s)) xs)
                rr (map (fn [x] (-reifyc x nil r a)) rs)
                xr (map (fn [x] (walk r x)) xs)
                smt-lines (concat (map (fn [x] `(~'declare-const ~x ~'Int)) xr)
                                  (map (fn [x] `(~'assert ~x)) rr))]
            (when (check-sat smt-lines)
              a)))
        IRunnable
        (-runnable? [_]
          true)))
    IConstraintOp
    (-rator [_] `-smtc)
    (-rands [_] xs)
    IReifiableConstraint
    (-reifyc [c v r s]
      (walk* r p))
    IConstraintWatchedStores
    (-watched-stores [this] #{::subst} #{::smt})))

(defn smtc [xs p]
  (cgoal (-smtc xs p)))

(run* [q]
  (smtc [q] `(~'= ~q 1))
  (smtc [q] `(~'= ~q 2))
  (smtc [q] `(~'= ~q 3)))

(run* [q]
  (smtc [q] `(~'= ~q 1)))
