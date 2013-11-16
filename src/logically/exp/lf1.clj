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
       (declare ~(symbol (str `~name '*)))
       ~@(map (fn [[tag [ps & spec]]]
                (let [vspec (vec spec)
                      tspec (vec (map (fn [s] (if (vector? s) (nth s 1) s)) vspec))
                      n (count tspec)
                      ty (nth tspec (- n 1))
                      ts (vec (map (fn [x s] (if (vector? s) (nth s 0) (gensym "x"))) (range 0 (- n 1)) vspec))]
                   `(do
                      (defn ~tag [~@ps ~@ts] (cons '~tag [~@ps ~@ts]))
                      (defn ~(symbol (str `~tag '-typechecks)) []
                        ~(if (= ty typ)
                           `true
                           `(not
                             (empty?
                              (run 1 [q#]
                                   (fresh [~@ps ~@ts]
                                          (~(symbol (str `~name '*))
                                           [[~ty ~typ] ~@(for [i (range 0 (- n 1))
                                                               :let [si (tspec i)]]
                                                           `[~si ~typ])])))))))
                      (defn ~(symbol (str `~tag '-clause)) [~c ~a ~b]
                        (fresh [~@ps ~@ts]
                               (== ~c (cons '~tag [~@ps ~@ts]))
                               (== ~a ~ty)
                               (== ~b [~@(for [i (range 0 (- n 1))
                                               :let [ti (ts i)
                                                     si (tspec i)]]
                                           `[~ti ~si])]))))))
              clauses)
       (defn ~(symbol (str `~name '-ok)) []
         (and
          ~@(map (fn [[tag _]]
                   `(if (~(symbol (str `~tag '-typechecks))) true
                        (do (println (str '~tag " clause does not typecheck."))
                            false)))
                 clauses)))
       (defn ~(symbol (str `~name '-clauses)) [~c ~a ~b]
         (conde
          ~@(map (fn [[tag _]]
                   `[(~(symbol (str `~tag '-clause)) ~c ~a ~b)])
                 clauses)))
       (def ~(symbol (str `~name '*)) (solve-for*  ~(symbol (str `~name '-clauses))))
       (def ~name (solve-for ~(symbol (str `~name '-clauses)))))))
