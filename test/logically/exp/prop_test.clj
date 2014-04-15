(ns logically.exp.prop_test  
  (:use [logically.exp.prop] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(deftest prop-modus-ponens
  (is (= (run* [q]
               (fresh [model pv qv]
                      (== model [['p pv] ['q qv]])
                      (prop-proof [:evalo [:implies [:and 'p [:implies 'p 'q]] 'q] false model] q)))
         '()))
  (is (= (run* [q]
               (fresh [model pv qv proof]
                      (== q [pv qv proof])
                      (== model [['p pv] ['q qv]])
                      (prop-proof [:evalo [:implies [:and 'p [:implies 'p 'q]] 'q] true model] proof)))
'([false
  _0
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p false] [q _0]]]
    <--
    ([[:evalo [:and p [:implies p q]] false [[p false] [q _0]]]
      <--
      ([[:evalo p false [[p false] [q _0]]] <-- ()])])])]
 [true
  false
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p true] [q false]]]
    <--
    ([[:evalo [:and p [:implies p q]] false [[p true] [q false]]]
      <--
      ([[:evalo p true [[p true] [q false]]] <-- ()]
       [[:evalo [:implies p q] false [[p true] [q false]]]
        <--
        ([[:evalo p true [[p true] [q false]]] <-- ()]
         [[:evalo q false [[p true] [q false]]] <-- ()])])])])]
 [true
  true
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p true] [q true]]]
    <--
    ([[:evalo [:and p [:implies p q]] true [[p true] [q true]]]
      <--
      ([[:evalo p true [[p true] [q true]]] <-- ()]
       [[:evalo [:implies p q] true [[p true] [q true]]]
        <--
        ([[:evalo p true [[p true] [q true]]] <-- ()]
         [[:evalo q true [[p true] [q true]]] <-- ()])])]
     [[:evalo q true [[p true] [q true]]] <-- ()])])])))

  )

(deftest prop-generate
  (is (=
       (run 6 [out]
            (fresh [fml model proof]
                   (== out [fml proof])
                   (== model [['p true] ['q false]])
                   (prop-proof [:evalo fml false model] proof)))
'([q ([[:evalo q false [[p true] [q false]]] <-- ()])]
 [[:not p]
  ([[:evalo [:not p] false [[p true] [q false]]]
    <--
    ([[:evalo p true [[p true] [q false]]] <-- ()])])]
 [[:not [:not q]]
  ([[:evalo [:not [:not q]] false [[p true] [q false]]]
    <--
    ([[:evalo [:not q] true [[p true] [q false]]]
      <--
      ([[:evalo q false [[p true] [q false]]] <-- ()])])])]
 [[:not [:not [:not p]]]
  ([[:evalo [:not [:not [:not p]]] false [[p true] [q false]]]
    <--
    ([[:evalo [:not [:not p]] true [[p true] [q false]]]
      <--
      ([[:evalo [:not p] false [[p true] [q false]]]
        <--
        ([[:evalo p true [[p true] [q false]]] <-- ()])])])])]
 [[:implies p q]
  ([[:evalo [:implies p q] false [[p true] [q false]]]
    <--
    ([[:evalo p true [[p true] [q false]]] <-- ()]
     [[:evalo q false [[p true] [q false]]] <-- ()])])]
 [[:not [:implies p p]]
  ([[:evalo [:not [:implies p p]] false [[p true] [q false]]]
    <--
    ([[:evalo [:implies p p] true [[p true] [q false]]]
      <--
      ([[:evalo p true [[p true] [q false]]] <-- ()]
       [[:evalo p true [[p true] [q false]]] <-- ()])])])]))))
