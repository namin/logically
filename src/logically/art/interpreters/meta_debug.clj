(ns logically.art.interpreters.meta_debug
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [logically.art.interpreters.meta]))

;; Section 17.3 Enhanced Meta-Interpreters for Debugging

;; Program 17.11 A meta-interpreter detecting a stack overflow
(defn debug-so-solve-for [clause]
  (letfn [(solve0 [goal depth overflow]
            (solve [goal] depth overflow))
          (solve [goals d o]
            (conde
              [(== goals ())
               (== o 'no-overflow)]
              [(!= goals ())
               (== d 0)
               (== o ['overflow ()])]
              [(fresh [g gs b db og ob]
                 (project [d]
                   (== (> d 0) true)
                   (== db (dec d)))
                 (conso g gs goals)
                 (clause g b)
                 (solve b db ob)
                 (return-overflow ob g og)
                 (solve-conjunction og gs d o))]))
          (solve-conjunction [og gs d o]
            (conde
              [(fresh [s]
                 (== og ['overflow s])
                 (== o ['overflow s]))]
              [(== og 'no-overflow)
               (solve gs d o)]))
          (return-overflow [ob g og]
            (conde
              [(== ob 'no-overflow)
               (== og 'no-overflow)]
              [(fresh [s]
                 (== ob ['overflow s])
                 (== og ['overflow (lcons g s)]))]))]
    solve0))

(def ex-debug-so-solver-member (debug-so-solve-for solver-member-clause))

;; Program 17.12 A non-terminating insertion sort
(defn solver-so-buggy-isort-clause [a b]
  (conde
    [(fresh [xs ys]
       (== a ['isort xs ys])
       (conde
         [(== xs ())
          (== ys ())
          (== b ())]
         [(fresh [x xt zs]
            (conso x xt xs)
            (== b [['isort xt zs] ['insert x zs ys]]))]))]
    [(fresh [x s r]
       (== a ['insert x s r])
       (conde
         [(fresh [y ys]
            (conso y ys s)
            (conde
              [(conso x s r)
               (project [x y]
                 (== (< x y) true))
               (== b ())]
              [(fresh [zs]
                 (conso y zs r)
                 (project [x y]
                   (== (>= x y) true))
                 (== b [['insert y (lcons x ys) zs]]))]))]
         [(== s ())
          (== r [x])
          (== b ())]))]))

(def ex-debug-so-buggy-isort-solver (debug-so-solve-for solver-so-buggy-isort-clause))

