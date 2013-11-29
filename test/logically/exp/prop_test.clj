(ns logically.exp.prop_test  
  (:use [logically.exp.prop] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest prop-modus-ponens
  (is (= (run* [q]
               (fresh [model pv qv]
                      (== model [['p pv] ['q qv]])
                      (prop-proof [:evalo [:implies [:and 'p [:implies 'p 'q]] 'q] false model] q)))
         '() ;; Can we find a meta-interpreter where we get a useful negative proof out?
         )))

(deftest prop-generate
  (is (=
       (run 6 [out]
            (fresh [fml model proof]
                   (== out [fml proof])
                   (== model [['p true] ['q false]])
                   (prop-proof [:evalo fml false model] proof)))
       '([q
         ([[:evalo q false [[p true] [q false]]] <-- ()])]
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
        [[:and q _0]
         ([[:evalo [:and q _0] false [[p true] [q false]]]
           <--
           ([[:evalo q false [[p true] [q false]]] <-- ()])])]
        [[:or q q]
         ([[:evalo [:or q q] false [[p true] [q false]]]
           <--
           ([[:evalo q false [[p true] [q false]]] <-- ()]
            [[:evalo q false [[p true] [q false]]] <-- ()])])]))))
