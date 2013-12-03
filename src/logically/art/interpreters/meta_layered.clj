(ns logically.art.interpreters.meta_layered
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn solve* [c goals proof ok]
  (conde
   [(== goals ())
    (== proof ())
    (== ok true)]
   [(fresh [g gs]
           (conso g gs goals)
           (fresh [bs proof-bs okb]
                  (conde
                   [(c g bs)
                    (solve* c bs proof-bs okb)]
                   [(== okb false)
                    (== proof-bs [:assume g false])])
                  (conde [(== okb true)
                          (fresh [proof-gs]
                                 (conso [g :<-- proof-bs] proof-gs proof)
                                 (solve* c gs proof-gs ok))]
                         [(== okb false)
                          (== ok false)
                          (== proof [:not g :<-- proof-bs])])))]))

(declare sanity-check-proof)

(defn solve-top [c goal proof ok]
  (all
   (solve* c [goal] proof ok)
   (sanity-check-proof c proof)))

(defn sanity-check-goal-false [c g]
  (fresh [g0 p0]
         (copy-term g g0)
         (conde
          [(solve* c [g0] p0 true)
           (!= g g0)]
          [(conda
            [(solve* c [g0] p0 true)
             fail]
            [succeed])])))

(defn sanity-check-proof [c proof]
  (conde
   [(== () proof)]
   [(fresh [g]
           (== [:assume g false] proof)
           (sanity-check-goal-false c g))]
   [(fresh [g p]
           (== [:not g :<-- p] proof)
           (sanity-check-goal-false c g)
           (sanity-check-proof c p))]
   [(fresh [g p ps]
           (conso [g :<-- p] ps proof)
           (sanity-check-proof c p)
           (sanity-check-proof c ps))]))
