(ns logically.exp.lf1
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn solve-for* [clause]
  (letfn [(solve [goals]
            (conde
             [(== goals ())]
             [(fresh [g gs c a b]
                     (conso g gs goals)
                     (== g [c a])
                     (clause c a b)
                     (solve b)
                     (solve gs))]))]
    solve))
(defn solve-for [clause]
  (let [solver* (solve-for* clause)]
    (fn [c a] (solver* [[c a]]))))

(def typ 'typ)
(defmacro defc [name & clauses]
  (let [c (gensym "c")
        a (gensym "a")
        b (gensym "b")]
    `(do
       ~@(map (fn [[tag [ps & spec]]]
                (let [vspec (vec spec)
                      n (count vspec)
                      ty (nth vspec (- n 1))
                      ts (vec (map (fn [x] (gensym "x")) (range 0 (- n 1))))]
                   `(do
                      ~(if (empty? ts)
                         `(def ~tag '~tag)
                         `(defn ~tag [~@ts] (cons '~tag [~@ts])))
                      (defn ~(symbol (str `~tag '-clause)) [~c ~a ~b]
                        (fresh [~@ps ~@ts]
                               (== ~c ~(if (empty? ts) `~tag `(~tag ~@ts)))
                               (== ~a ~ty)
                               (== ~b [~@(for [i (range 0 (- n 1))
                                               :let [ti (ts i)
                                                     si (vspec i)]]
                                           `[~ti ~si])]))))))
              clauses)
       (defn ~(symbol (str `~name '-clauses)) [~c ~a ~b]
         (conde
          ~@(map (fn [[tag _]]
                   `[(~(symbol (str `~tag '-clause)) ~c ~a ~b)])
                 clauses)))
       (def ~name (solve-for ~(symbol (str `~name '-clauses)))))))

(defc naturals
  [nat       [[] typ]]
  [z         [[] nat]]
  [s         [[] nat nat]]
  [plus      [[] nat nat nat typ]]
  [plus-z    [[n#] (plus z n# n#)]]
  [plus-s    [[n1# n2# n3#] (plus n1# n2# n3#) (plus (s n1#) n2# (s n3#))]]
  [sum-inc   [[n1# n2# n3#] (plus n1# n2# n3#) (plus n1# (s n2#) (s n3#)) typ]]
  [sum-inc-z [[] (sum-inc plus-z plus-z)]]
  [sum-inc-s [[d1# d2#] (sum-inc d1# d2#) (sum-inc (plus-s d1#) (plus-s d2#))]])
