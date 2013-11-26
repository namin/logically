(ns logically.abs.induced_tp_test
  (:use [logically.abs.induced_tp] :reload)
  (:use [logically.abs.ex_ack] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-ack
  (let [r (set (run* [q] (fresh [res] (go ack-clause [:ack [:s [:s 0]] [:s [:s 0]] res] q))))]
    (is (contains? r
                   [:ans
                    [:ack
                     [:s [:s 0]]
                     [:s [:s 0]]
                     [:s [:s [:s [:s [:s [:s [:s 0]]]]]]]]]))))
