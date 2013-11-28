(ns logically.abs.induced_tp_test
  (:use [logically.abs.induced_tp] :reload)
  (:use [logically.abs.ex_ack] :reload)
  (:use [logically.abs.ex_rotate] :reload)
  (:use [logically.abs.db] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(def s-ack
  '(
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
      ))

(def s-rotate
  '(
    [:call [:rotate (a b c) _0]]
    [:call [:append _0 _1 (a b c)]]
    [:ans [:append () (a b c) (a b c)]]
    [:call [:append _0 _1 (b c)]]
    [:call [:append (a b c) () _0]]
    [:ans [:append () (b c) (b c)]]
    [:ans [:append (a) (b c) (a b c)]]
    [:call [:append _0 _1 (c)]]
    [:call [:append (b c) () _0]]
    [:call [:append (b c) (a) _0]]
    [:ans [:append () (c) (c)]]
    [:ans [:append (b) (c) (b c)]]
    [:call [:append _0 _1 ()]]
    [:call [:append (c) () _0]]
    [:call [:append (c) (a) _0]]
    [:ans [:append () () ()]]
    [:ans [:append (a b) (c) (a b c)]]
    [:ans [:append (c) () (c)]]
    [:ans [:append (b c) () (b c)]]
    [:call [:append () () _0]]
    [:call [:append () (a) _0]]
    [:call [:append (c) (a b) _0]]
    [:ans [:append () (a) (a)]]
    [:ans [:append (a b c) () (a b c)]]
    [:ans [:append (c) (a) (c a)]]
    [:call [:append () (a b) _0]]
    [:ans [:rotate (a b c) (a b c)]]
    [:call [:append () (a b c) _0]]
    [:ans [:append () (a b) (a b)]]
    [:ans [:append (b c) (a) (b c a)]]
    [:ans [:append (c) (a b) (c a b)]]
    [:ans [:rotate (a b c) (b c a)]]
    [:ans [:rotate (a b c) (c a b)]]
      ))

(deftest test-ack
  (let [r (reverse (run* [q] (fresh [res] (go ack-clause [:ack [:s [:s 0]] [:s [:s 0]] res] q))))]
    (is (= (set r) (set s-ack)))))

(deftest test-rotate
  (let [r (reverse (run* [q] (fresh [res] (go rotate-clause [:rotate '(a b c) res] q))))]
    (is (= (set r) (set s-rotate)))))
