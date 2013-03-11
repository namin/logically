(ns logically.ai.meta.ebg_test
  (:use [logically.ai.meta.ebg] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-ex-gift-domain
  (is (= (run* [q]
           (ex-gift-domain-solver ['gives 'john 'john 'chocolate]))
        '(_0)))
  (is (= (run* [q]
           (ex-gift-domain-solver ['gives 'john 'annie 'chocolate]))
        '()))
  (is (= (run 1 [q]
           (fresh [x y z cs]
             (ex-gift-domain-ebg ['gives 'john 'john 'chocolate] ['gives x y z] cs)
             (== q [x y z cs])))
        '([_0 _0 _1 ([sad _0] [likes _0 _1])]))))

(deftest test-ex-lift-domain
  (is (= (run 1 [moves]
           (ex-lift-domain-solver ['go 3 6 moves]))
        '((up up up))))
  (is (= (run 1 [q]
           (fresh [moves level1 level2 gen-moves conditions]
             (ex-lift-domain-ebg ['go 3 6 moves] ['go level1 level2 gen-moves] conditions)
             (== q [moves gen-moves level1 level2 conditions])))
        '([(up up up) (up up up) _0 _1 ([[[[0 + 1] + 1] + 1] =:= _1 - _0])]))))
