(ns logically.art.interpreters.fsm
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

;; Section 17.1 Finite State Machines

;; Program 17.1 An interpreter for a nondeterministic finite automaton (NDFA)
(defn create-ndfa [initial final delta]
  (letfn [(accept0 [xs]
            (fresh [q]
              (initial q)
              (accept xs q)))
          (accept [xs q]
            (conde
              [(fresh [x xs1 q1]
                 (conso x xs1 xs)
                 (delta q x q1)
                 (accept xs1 q1))]
              [(== xs ())
               (final q)]))]
    accept0))

;; Program 17.2 An NDFA that accepts the language (ab)*
(def ex-ndfa-1
  (letfn [(initial [s] (== s 'q0))
          (final [s] (== s 'q0))
          (delta [qa x qb]
            (conde
              [(== [qa x qb] ['q0 'a 'q1])]
              [(== [qa x qb] ['q1 'b 'q0])]))]
    (create-ndfa initial final delta)))

;; Program 17.3 An interpreter for a nondeterministic pushdown automaton (NPDA)
(defn create-npda [initial final delta]
  (letfn [(accept0 [xs]
            (fresh [q]
              (initial q)
              (accept xs q ())))
          (accept [xs q s]
            (conde
              [(fresh [x xs1 q1 s1]
                 (conso x xs1 xs)
                 (delta q x s q1 s1)
                 (accept xs1 q1 s1))]
              [(== xs ())
               (== s ())
               (final q)]))]
    accept0))

;; Program 17.4 An NPDA for palindromes over a finite alphabet
(def ex-npda-palindromes
  (letfn [(initial [s] (== s 'q0))
          (final [s] (== s 'q1))
          (delta [qa x sa qb sb]
            (conde
              [(== [qa qb] ['q0 'q0])
               (conso x sa sb)]
              [(== [qa qb] ['q0 'q1])
               (conde
                 [(conso x sa sb)]
                 [(== sa sb)])]
              [(== [qa qb] ['q1 'q1])
               (conso x sb sa)]))]
    (create-npda initial final delta)))
