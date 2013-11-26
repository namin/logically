(ns logically.abs.induced_tp_test
  (:use [logically.abs.induced_tp] :reload)
  (:use [logically.abs.ex_ack] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(def s
  '#{
     [:call [:ack [:s [:s 0]] [:s [:s 0]] _0]]
     [:call [:ack [:s [:s 0]] [:s 0] _0]]
     [:call [:ack [:s [:s 0]] 0 _0]]
     [:call [:ack [:s 0] [:s 0] _0]]
     [:call [:ack [:s 0] 0 _0]]
     [:call [:ack 0 [:s 0] _0]]
     [:ans [:ack 0 [:s 0] [:s [:s 0]]]]
     [:ans [:ack [:s 0] 0 [:s [:s 0]]]]
     [:call [:ack 0 [:s [:s 0]] _0]]
     [:ans [:ack 0 [:s [:s 0]] [:s [:s [:s 0]]]]]
     [:ans [:ack [:s 0] [:s 0] [:s [:s [:s 0]]]]]
     [:ans [:ack [:s [:s 0]] 0 [:s [:s [:s 0]]]]]
     [:call [:ack [:s 0] [:s [:s [:s 0]]] _0]]
     [:call [:ack [:s 0] [:s [:s 0]] _0]]
     [:call [:ack 0 [:s [:s [:s 0]]] _0]]
     [:ans [:ack 0 [:s [:s [:s 0]]] [:s [:s [:s [:s 0]]]]]]
     [:ans [:ack [:s 0] [:s [:s 0]] [:s [:s [:s [:s 0]]]]]]
     [:call [:ack 0 [:s [:s [:s [:s 0]]]] _0]]
     [:ans [:ack 0 [:s [:s [:s [:s 0]]]] [:s [:s [:s [:s [:s 0]]]]]]]
     [:ans [:ack [:s 0] [:s [:s [:s 0]]] [:s [:s [:s [:s [:s 0]]]]]]]
     [:ans [:ack [:s [:s 0]] [:s 0] [:s [:s [:s [:s [:s 0]]]]]]]
     [:call [:ack [:s 0] [:s [:s [:s [:s [:s 0]]]]] _0]]
     [:call [:ack [:s 0] [:s [:s [:s [:s 0]]]] _0]]
     [:call [:ack 0 [:s [:s [:s [:s [:s 0]]]]] _0]]
     [:ans [:ack 0 [:s [:s [:s [:s [:s 0]]]]] [:s [:s [:s [:s [:s [:s 0]]]]]]]]
     [:ans [:ack [:s 0] [:s [:s [:s [:s 0]]]] [:s [:s [:s [:s [:s [:s 0]]]]]]]]
     [:call [:ack 0 [:s [:s [:s [:s [:s [:s 0]]]]]] _0]]
     [:ans [:ack 0 [:s [:s [:s [:s [:s [:s 0]]]]]] [:s [:s [:s [:s [:s [:s [:s 0]]]]]]]]]
     [:ans [:ack [:s 0] [:s [:s [:s [:s [:s 0]]]]] [:s [:s [:s [:s [:s [:s [:s 0]]]]]]]]]
     [:ans [:ack [:s [:s 0]] [:s [:s 0]] [:s [:s [:s [:s [:s [:s [:s 0]]]]]]]]]
     })

(deftest test-ack
  (let [r (set (run* [q] (fresh [res] (go ack-clause [:ack [:s [:s 0]] [:s [:s 0]] res] q))))]
    (is (= s r))))
