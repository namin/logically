(ns logically.exp.lf1
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [logically.art.interpreters.meta]))

(defmacro defr- [name typ pparams & variants]
  `(do
     ~(let [n# (if (vector? typ) (count typ) 1)
            args (vec
                  (for [i# (range 0 (- n# 1))
                        :let [t# (typ i#)]]
                    (if (and (vector? t#) (= :- (first t#)))
                      [(nth t# 1) (nth t# 2)]
                      [(symbol (str 'x i#)) t#])))
            ot (if (vector? typ) (typ (- n# 1)) typ)
            params (map first args)]
        `(do
           (declare ~(symbol (str `~name (if (empty? variants) '-v '-o))))
           ~@(map (fn [[vname vtyp]]
                    `(defr- ~vname ~vtyp ~params)) variants)
           ~(if (empty? params)
              `(def ~name '~name)
              `(defn ~name [~@params]
                 (cons '~name [~@params])))
           ~(if (empty? variants)
              `(defn ~(symbol (str `~name '-v)) [~@pparams o#]
                 (fresh [~@params]
                        (== o# ~(if (empty? params) `~name `(cons '~name [~@params])))
                        ~@(map (fn [p a]
                                 `(== ~p ~a))
                               pparams (if (empty? pparams) [] `[~@(rest ot)]))
                        ~@(map (fn [[p t]]
                                 (if (symbol? `~t)
                                   `(~(symbol (str `~t '-o)) ~p)
                                   `(~(symbol (str (first `~t) '-o)) ~@(rest t) ~p)))
                               args)))
              (let [o (gensym "o")]
                `(defn ~(symbol (str `~name '-o)) [~@params ~o]
                   (conde
                    ~@(map (fn [[vname vtyp]] `[(~(symbol (str `~vname '-v)) ~@params ~o)]) variants)))))))))

(defmacro defr [name typ & variants]
  `(defr- ~name ~typ nil ~@variants))

(defn lf-type-o [q]
  succeed)

(defr nat lf-type
  [z nat]
  [s [nat nat]])

(defr plus [nat nat nat lf-type]
  [plus-z [[:- n2 nat] (plus z n2 n2)]]
  [plus-s [[:- n1 nat] [:- n2 nat] [:- n3 nat]
           (plus n1 n2 n3)
           (plus (s n1) n2 (s n3))]])

(defr sum-inc [[:- xn1 nat] [:- xn2 nat] [:- xn3 nat]
               (plus xn1 xn2 xn3)
               (plus xn1 (s xn2) (s xn3))
               lf-type]
  [sum-inc-z [[:- n1 nat] [:- n2 nat] [:- n3 nat]
              (sum-inc n1 n2 n3 (plus-z n2) (plus-z (s n2)))]]
  [sum-inc-s [[:- n1 nat] [:- n2 nat] [:- n3 nat]
              [:- pi (plus n1 n2 n3)]
              [:- po (plus n1 (s n2) (s n3))]
              (sum-inc n1 n2 n3 pi po)
              (sum-inc (s n1) n2 (s n3)
                       (plus-s (s n1) n2 (s n3) pi)
                       (plus-s (s (s n1)) n2 (s (s n3)) po))]])
