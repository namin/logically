(ns logically.nominal.nsl
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
)

;; Inspired by Cryptographic authentication protocols
;; (Needham-Schroeder(-Lowe)) example of alphaProlog
;; http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/

(defn sendo [m l ms]
  (conso m l ms))

(defn recvo [m l]
  (membero m l))

(defn fresho [n l]
  (nom/fresh [tok]
    (nom/hash tok l)
    (== tok n)))

(defn deco [m1 k m2]
  (fresh [a]
    (== ['enc ['pub a] m2] m1)
    (== ['pri a] k)))

(defn ns-step1-o [a b ms1 ms2]
  (fresh [m]
    (fresho m ms1)
    (sendo ['enc ['pub b] ['pair ['nonce m] ['nm a]]] ms1 ms2)))

(defn ns-step2-o [b a ms1 ms2]
  (fresh [m n msg]
    (recvo msg ms1)
    (deco msg ['pri b] ['pair ['nonce m] ['nm a]])
    (fresho n ms1)
    (sendo ['enc ['pub a] ['pair ['nonce m] ['nonce n]]] ms1 ms2)))

(defn ns-step3-o [a b ms1 ms2]
  (fresh [m n msg]
    (recvo msg ms1)
    (deco msg ['pri a] ['pair ['nonce m] ['nonce n]])
    (sendo ['enc ['pub b] ['nonce n]] ms1 ms2)))

(defn ns-typicalo [msa msb]
  (fresh [ms1 ms2]
    (ns-step1-o 'alice 'bob msa ms1)
    (ns-step2-o 'bob 'alice ms1 ms2)
    (ns-step3-o 'alice 'bob ms2 msb)))

(defn ns-attacko [msa msb]
  (fresh [m1 n1 m3 n3 ms1 ms2 ms3 ms4]
    (ns-step1-o 'alice 'eve msa ms1)
    (recvo m1 ms1)
    (deco m1 ['pri 'eve] n1)
    (sendo ['enc ['pub 'bob] n1] ms1 ms2)
    (ns-step2-o 'bob 'alice ms2 ms3)
    (ns-step3-o 'alice 'eve ms3 ms4)
    (recvo m3 ms4)
    (deco m3 ['pri 'eve] n3)
    (sendo ['enc ['pub 'bob] n3] ms4 msb)))

(defn nsl-step1-o [a b ms1 ms2]
  (fresh [m]
    (fresho m ms1)
    (sendo ['enc ['pub b] ['pair ['nonce m] ['nm a]]] ms1 ms2)))

(defn nsl-step2-o [b a ms1 ms2]
  (fresh [m n msg]
    (recvo msg ms1)
    (deco msg ['pri b] ['pair ['nonce m] ['nm a]])
    (fresho n ms1)
    (sendo ['enc ['pub a] ['pair ['pair ['nonce m] ['nonce n]] ['nm b]]] ms1 ms2)))

(defn nsl-step3-o [a b ms1 ms2]
  (fresh [m n msg]
    (recvo msg ms1)
    (deco msg ['pri a] ['pair ['pair ['nonce m] ['nonce n]] ['nm b]])
    (sendo ['enc ['pub b] ['nonce n]] ms1 ms2)))

(defn nsl-typicalo [msa msb]
  (fresh [ms1 ms2]
    (nsl-step1-o 'alice 'bob msa ms1)
    (nsl-step2-o 'bob 'alice ms1 ms2)
    (nsl-step3-o 'alice 'bob ms2 msb)))

(defn nsl-attacko [msa msb]
  (fresh [m1 n1 m3 n3 ms1 ms2 ms3 ms4]
    (nsl-step1-o 'alice 'eve msa ms1)
    (recvo m1 ms1)
    (deco m1 ['pri 'eve] n1)
    (sendo ['enc ['pub 'bob] n1] ms1 ms2)
    (nsl-step2-o 'bob 'alice ms2 ms3)
    (nsl-step3-o 'alice 'eve ms3 ms4)
    (recvo m3 ms4)
    (deco m3 ['pri 'eve] n3)
    (sendo ['enc ['pub 'bob] n3] ms4 msb)))
