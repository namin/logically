(ns logically.abs.tp_abs_test
  (:use [logically.abs.tp_abs] :reload)
  (:use [logically.abs.unif] :reload)
  (:use [logically.abs.ex_plus] :reload)
  (:use [logically.abs.ex_ack] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-impl
  (is (= (set (run* [q]
                    (go u==-parity
                        (fn [head body]
                          (fresh [a b x]
                                 (== head [:foo a b])
                                 (== body [])))
                        q)))
         '#{[:foo _0 _1]}))

  (is (= (set (run* [q]
                    (go u==-parity
                        (fn [head body]
                          (fresh [a b x]
                                 (== head [:foo a b])
                                 (== body [[:== a [:s 0]]])))
                        q)))
         '#{[:foo :odd _0]}))

    (is (= (set (run* [q]
                    (go u==-parity
                        (fn [head body]
                          (fresh [a b x]
                                 (== head [:foo a b])
                                 (== body [[:== a [:s x]]])))
                        q)))
           '#{[:foo :odd _0] [:foo :even _0]}))

    (is (= (set (run* [q]
                      (go u==-parity
                          (fn [head body]
                            (fresh [a b x]
                                   (== head [:foo a b])
                                   (== body [[:== a [:s x]]
                                             [:== b x]])))
                        q)))
           '#{[:foo :odd :even] [:foo :even :odd]}))

    (is (= (set (run 1 [q]
                      (go u==-parity
                          (fn [head body]
                            (fresh [a b x]
                                   (== head [:foo a b])
                                   (== body [[:== a x]])))
                        q)))
           '#{[:foo _0 _1]}))

    (is (= (set (run 1 [q]
                     (go (fn [x y]
                           (== x y))
                          (fn [head body]
                            (fresh [a b x]
                                   (== head [:foo a b])
                                   (== body [[:== a x]
                                             [:== b x]])))
                        q)))
         '#{[:foo _0 _0]}))

  )

(deftest test-plus
  (is (= (set (run* [q] (go u==-parity plus-norm-clause q)))
         '#{
            [:plus :even _0 _0]
            [:plus :odd :even :odd]
            [:plus :odd :odd :even]
            [:times :even _0 :even]
            [:times :odd _0 _0]
            })))

(deftest test-ack
  (is (= (set (run* [q] (go u==-parity1 ack-norm-clause q)))
         '#{
            [:ack :zero :odd :even]
            [:ack :zero :even :odd]
            [:ack :zero :one :even]
            [:ack :zero :zero :one]
            [:ack :one :zero :even]
            [:ack :one :one :odd]
            [:ack :even :zero :odd]
            [:ack :one :even :even]
            [:ack :one :odd :odd]
            [:ack :even :one :odd]
            [:ack :odd :zero :odd]
            [:ack :even :even :odd]
            [:ack :even :odd :odd]
            [:ack :odd :one :odd]
            [:ack :odd :even :odd]
            [:ack :odd :odd :odd]
            }
         )))
