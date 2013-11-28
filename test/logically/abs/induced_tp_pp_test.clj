(ns logically.abs.induced_tp_pp_test
  (:use [logically.abs.induced_tp_pp] :reload)
  (:use [logically.abs.ex_ack] :reload)
  (:use [logically.abs.ex_rotate] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(def s-rotate
  '#{
    [:call 0 0 [:rotate (a b c) _0]]
    [:call 1 1 [:append _0 _1 (a b c)]]
    [:ans [:append () (a b c) (a b c)]]
    [:call 3 1 [:append _0 _1 (b c)]]
    [:call 1 2 [:append (a b c) () _0]]
    [:ans [:append () (b c) (b c)]]
    [:ans [:append (a) (b c) (a b c)]]
    [:call 3 1 [:append _0 _1 (c)]]
    [:call 3 1 [:append (b c) () _0]]
    [:call 1 2 [:append (b c) (a) _0]]
    [:ans [:append () (c) (c)]]
    [:ans [:append (b) (c) (b c)]]
    [:call 3 1 [:append _0 _1 ()]]
    [:call 3 1 [:append (c) () _0]]
    [:call 3 1 [:append (c) (a) _0]]
    [:ans [:append () () ()]]
    [:ans [:append (a b) (c) (a b c)]]
    [:ans [:append (c) () (c)]]
    [:ans [:append (b c) () (b c)]]
    [:call 3 1 [:append () () _0]]
    [:call 3 1 [:append () (a) _0]]
    [:call 1 2 [:append (c) (a b) _0]]
    [:ans [:append () (a) (a)]]
    [:ans [:append (a b c) () (a b c)]]
    [:ans [:append (c) (a) (c a)]]
    [:call 3 1 [:append () (a b) _0]]
    [:ans [:rotate (a b c) (a b c)]]
    [:call 1 2 [:append () (a b c) _0]]
    [:ans [:append () (a b) (a b)]]
    [:ans [:append (b c) (a) (b c a)]]
    [:ans [:append (c) (a b) (c a b)]]
    [:ans [:rotate (a b c) (b c a)]]
    [:ans [:rotate (a b c) (c a b)]]
    })

(deftest test-rotate
  (let [r (set (run* [q] (fresh [res] (go rotate-pp-clause [:rotate '(a b c) res] q))))]
    (is (= r s-rotate))))
