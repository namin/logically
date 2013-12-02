(ns logically.exp.prop
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.art.interpreters.meta])
  (:use [logically.exp.utils]))

;;; Translated from mk
;;; mk version was created by @webyrd and @jtfmumm at Hacker School

(defn asserto [prop truth model]
  (all
   (symo prop)
   (conde
    [(fresh [rest]
            (conso [prop truth] rest model))]
    [(fresh [pr tr rest]
            (conso [pr tr] rest model)
            (!= pr prop)
            (asserto prop truth rest))])))

(defn evalo [fml truth model]
  (conde
   [(symo fml)
    (asserto fml truth model)]
   [(fresh [fm]
           (== [:not fm] fml)
           (conde
            [(== true truth)
             (evalo fm false model)]
            [(== false truth)
             (evalo fm true model)]))]
   [(fresh [fm1 fm2]
           (== [:implies fm1 fm2] fml)
           (conde
            [(== true truth)
             (conde
              [(evalo fm1 true model)
               (evalo fm2 true model)]
              [(evalo fm1 false model)])]))]
   [(fresh [fm1 fm2]
           (== [:and fm1 fm2] fml)
           (conde
            [(== true truth)
             (evalo fm1 true model)
             (evalo fm2 true model)]
            [(== false truth)
             (conde
              [(evalo fm1 false model)]
              [(evalo fm1 true model)
               (evalo fm2 false model)])]))]
   [(fresh [fm1 fm2]
           (== [:or fm1 fm2] fml)
           (conde
            [(== true truth)
             (conde
              [(evalo fm1 true model)]
              [(evalo fm1 false model)
               (evalo fm2 true model)])]
            [(== false truth)
             (evalo fm1 false model)
             (evalo fm2 false model)]))]))

(defn prop-clause [head body]
  (fresh [fml truth model]
         (== head [:evalo fml truth model])
         (conde
          [(symo fml)
           (asserto fml truth model)
           (== body [])]
          [(fresh [fm]
                  (== [:not fm] fml)
                  (conde
                   [(== true truth)
                    (== body [[:evalo fm false model]])]
                   [(== false truth)
                    (== body [[:evalo fm true model]])]))]
          [(fresh [fm1 fm2]
                  (== [:implies fm1 fm2] fml)
                  (conde
                   [(== true truth)
                    (conde
                     [(== body [[:evalo fm1 true model]
                                [:evalo fm2 true model]])]
                     [(== body [[:evalo fm1 false model]])])]))]
          [(fresh [fm1 fm2]
                  (== [:and fm1 fm2] fml)
                  (conde
                   [(== true truth)
                    (== body [[:evalo fm1 true model]
                              [:evalo fm2 true model]])]
                   [(== false truth)
                    (conde
                     [(== body [[:evalo fm1 false model]])]
                     [(== body [[:evalo fm1 true model]
                                [:evalo fm2 false model]])])]))]
          [(fresh [fm1 fm2]
                   (== [:or fm1 fm2] fml)
                   (conde
                    [(== true truth)
                     (conde
                      [(== body [[:evalo fm1 true model]])]
                      [(== body [[:evalo fm1 false model]
                                 [:evalo fm2 true model]])])]
                    [(== false truth)
                     (== body [[:evalo fm1 false model]
                               [:evalo fm2 false model]])]))])))

(def prop-proof (solve-proof-for prop-clause))
