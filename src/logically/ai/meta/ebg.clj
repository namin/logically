(ns logically.ai.meta.ebg
  (:use [logically.art.interpreters.meta])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

;; Section 25.6 Explanation-based generalization

(defn ebg-for* [clause operational]
  (let [solver (solve-for clause)]
    (letfn [(ebg [goals gengoals conditions]
              (conde
                [(== goals ())
                  (== gengoals ())
                  (== conditions ())]
                [(fresh [goal gs gengoal gengs hcs tcs]
                   (conso goal gs goals)
                   (conso gengoal gengs gengoals)
                   (conde
                     [(operational gengoal true)
                       (== hcs [gengoal])
                       (solver goal)]
                     [(operational goal false)
                       (fresh [cgoal cbody body genbody]
                         (clause gengoal genbody)
                         (copy-term (lcons gengoal genbody) (lcons goal body))
                         (ebg body genbody hcs))])
                   (appendo hcs tcs conditions)
                   (ebg gs gengs tcs))]))]
      ebg)))

(defn ebg-for [clause operational]
  (let [ebg* (ebg-for* clause operational)]
    (fn [goal gengoal conditions] (ebg* [goal] [gengoal] conditions))))

(defn gift-domain-clause [a b]
  (conde
    [(conde
       [(== a ['likes 'john 'annie])]
       [(== a ['likes 'annie 'john])]
       [(== a ['likes 'john 'chocolate])]
       [(== a ['needs 'annie 'tennis-racket])]
       [(== a ['sad 'john])])
      (== b ())]
    [(fresh [person1 person2 gift]
       (== a ['gives person1 person2 gift])
       (== b [['likes person1 person2]
              ['would-please gift person2]]))]
    [(fresh [person1 person2 gift]
       (== a ['gives person1 person2 gift])
       (== b [['feels-sorry-for person1 person2]
              ['would-comfort gift person2]]))]
    [(fresh [gift person]
       (== a ['would-please gift person])
       (== b [['needs person gift]]))]
    [(fresh [gift person]
       (== a ['would-comfort gift person])
       (== b [['likes person gift]]))]
    [(fresh [person1 person2]
       (== a ['feels-sorry-for person1 person2])
       (== b [['likes person1 person2]
              ['sad person2]]))]
    [(fresh [person]
       (== a ['feels-sorry-for person person])
       (== b [['sad person]]))]))

(defn gift-domain-operational [goal isop]
  (conda
    [(fresh [x y]
       (conde
         [(== goal ['likes x y])]
         [(== goal ['needs x y])]
         [(== goal ['sad x])]))
      (== isop true)]
    [(== isop false)]))

(def ex-gift-domain-solver (solve-for gift-domain-clause))
(def ex-gift-domain-ebg (ebg-for gift-domain-clause gift-domain-operational))

(defn calculate [x xn]
  (conda
    [(fresh [yn1 yn2 ys]
       (== [ys '+ yn1] x)
       (calculate ys yn2)
       (project [yn1 yn2]
         (== (+ yn1 yn2) xn)))]
    [(project [x]
       (== x xn))]))

(defn lift-domain-clause [a b]
  (conde
    [(fresh [level goal-level moves distance]
       (== a ['go level goal-level moves])
       (== b [['move-list moves distance]
              [distance '=:= goal-level '- level]]))]
    [(== a ['move-list () 0])
     (== b ())]
    [(fresh [move1 moves distance distance1]
       (== a ['move-list (lcons move1 moves) [distance '+ distance1]])
       (== b [['move move1 distance1]
              ['move-list moves distance]]))]
    [(== a ['move 'up 1])
     (== b ())]
    [(== a ['move 'down -1])
     (== b ())]
    [(fresh [x xn yn zn]
       (== a [x '=:= yn '- zn])
       (calculate x xn)
       (project [xn yn zn]
         (== xn (- yn zn))))]))

(defn lift-domain-operational [goal isop]
  (conda
    [(fresh [distance goal-level level]
       (== goal [distance '=:= goal-level '- level]))
     (== isop true)]
    [(== isop false)]))

(def ex-lift-domain-solver (solve-for lift-domain-clause))
(def ex-lift-domain-ebg (ebg-for lift-domain-clause lift-domain-operational))
