(ns logically.nominal.diff
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
)

;; Inspired by Symbolic differentiation example of alphaProlog
;; http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/

;; TODO(namin): Should this goal be part of core.logic.nominal?
(defn swapo [a b t out]
  (fn [s]
    (let [[t s] (swap-noms t [a b] s)]
      (bind* s (== t out)))))

(defn numbero [x]
  (predc x number? `number?))
 
(defn nomo [x]
  (predc x nom? `nomo?))

(defn diffo [x e ep]
  (conde
    [(numbero e) (== 0 ep)]
    [(nomo e) (== x e) (== 1 ep)]
    [(nomo e) (!= x e) (== e ep)]
    [(fresh [e1 e2 e1p e2p]
       (== ['+ e1 e2] e)
       (== ['+ e1p e2p] ep)
       (diffo x e1 e1p)
       (diffo x e2 e2p))]
    [(fresh [e1 e2 e1p e2p]
       (== ['* e1 e2] e)
       (== ['+ ['* e1p e2] ['* e1 e2p]] ep)
       (diffo x e1 e1p)
       (diffo x e2 e2p))]
    [(fresh [e1 e1p]
       (== ['sin e1] e)
       (== ['* ['cos e1] e1p] ep)
       (diffo x e1 e1p))]
    [(fresh [e1 e1p]
       (== ['cos e1] e)
       (== ['* ['- ['sin e1]] e1p] ep)
       (diffo x e1 e1p))]
    [(fresh [e1 e1p]
       (== ['- e1] e)
       (== ['- e1p] ep)
       (diffo x e1 e1p))]
    ;; fundamental theorem of calculus, special case
    [(fresh [e1 low high]
       (nom/fresh [y]
         (== ['integ (nom/tie y e1) low high] e)
         (numbero low)
         (== x high)
         (conde
           [(== x y) (== e1 ep)]
           [(!= x y)
            (nom/hash x e1)
            (swapo x y e1 ep)])))]))

(defn simplo [e out]
  (conde
    [(fresh [e1]
       (conde
         [(== e ['* e1 0])]
         [(== e ['* 0 e1])])
       (== 0 out))]
    [(fresh [e1]
       (conde
         [(== e ['+ e1 0])]
         [(== e ['+ 0 e1])])
       (simplo e1 out))]
    [(fresh [e1]
       (conde
         [(== e ['* e1 1])]
         [(== e ['* 1 e1])])
       (simplo e1 out))]
    [(fresh [e1 e1out]
       (== e ['- e1out])
       (simplo e1out ['- e1])
       (simplo e1 out))]
    [(fresh [e1 e1out e2 e2out]
       (== ['+ e1 e2] e)
       (== ['+ e1out e2out] out)
       (simplo e1 e1out)
       (simplo e2 e2out))]
    [(== e out)]))

(defn not-integ? [x]
  (not= 'integ x))
(defn no-integro [e]
  (treec e #(predc % not-integ? `not-integ?) `not-integ?))

;; Run it backwards!
(defn integro [x ep e]
  (fresh [ei c]
    (treec e #(predc % not-integ? `not-integ?) `not-integ?)
    (diffo x e ei)
    (simplo ei ep)))
