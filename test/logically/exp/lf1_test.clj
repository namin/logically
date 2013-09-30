(ns logically.exp.lf1_test
  (:use [logically.exp.lf1] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-naturals
  (is (naturals-ok))
  (is (= (run* [q] (fresh [a b c] (== q [c a b]) (naturals-clauses c a b) (== a typ)))
         '([(nat) typ []]
           [(plus _0 _1 _2) typ [[_0 (nat)] [_1 (nat)] [_2 (nat)]]]
           [(sum-inc _0 _1 _2 _3 _4)
            typ
            [[_3 (plus _0 _1 _2)] [_4 (plus _0 (s _1) (s _2))]]]
           [(plus-eq-deriv _0 _1 _2 _3 _4 _5 _6 _7)
            typ
            [[_6 (plus _0 _1 _2)] [_7 (plus _3 _4 _5)]]]
           [(plus-uniq-deriv _0 _1 _2 _3 _4 _5)
            typ
            [[_3 (plus _0 _1 _2)]
             [_4 (plus _0 _1 _2)]
             [_5 (plus-eq-deriv _0 _1 _2 _0 _1 _2 _3 _4)]]])))
  (is (= (run* [q] (fresh [a b c] (== q [c a b]) (naturals-clauses c a b)))
         '([(nat) typ []]
           [(z) (nat) []]
           [(s _0) (nat) [[_0 (nat)]]]
           [(plus _0 _1 _2) typ [[_0 (nat)] [_1 (nat)] [_2 (nat)]]]
           [(plus-z _0) (plus (z) _0 _0) []]
           [(plus-s _0 _1 _2 _3) (plus (s _0) _1 (s _2)) [[_3 (plus _0 _1 _2)]]]
           [(sum-inc _0 _1 _2 _3 _4)
            typ
            [[_3 (plus _0 _1 _2)] [_4 (plus _0 (s _1) (s _2))]]]
           [(sum-inc-z _0) (sum-inc _0 _0 _0 (plus-z _0) (plus-z (s _0))) []]
           [(sum-inc-s _0 _1 _2 _3 _4 _5)
            (sum-inc
             (s _0)
             _1
             (s _2)
             (plus-s _0 _1 _2 _3)
             (plus-s _0 (s _1) (s _2) _4))
            [[_5 (sum-inc _0 _1 _2 _3 _4)]]]
           [(plus-eq-deriv _0 _1 _2 _3 _4 _5 _6 _7)
            typ
            [[_6 (plus _0 _1 _2)] [_7 (plus _3 _4 _5)]]]
           [(plus-eq-deriv-z _0 _1)
            (plus-eq-deriv (z) _0 _0 (z) _1 _1 (plus-z _0) (plus-z _1))
            []]
           [(plus-eq-deriv-s _0 _1 _2 _3 _4 _5 _6 _7 _8)
            (plus-eq-deriv
             (s _0)
             _1
             (s _2)
             (s _3)
             _4
             (s _5)
             (plus-s _0 _1 _2 _6)
             (plus-s _3 _4 _5 _7))
            [[_8 (plus-eq-deriv _0 _1 _2 _3 _4 _5 _6 _7)]]]
           [(plus-uniq-deriv _0 _1 _2 _3 _4 _5)
            typ
            [[_3 (plus _0 _1 _2)]
             [_4 (plus _0 _1 _2)]
             [_5 (plus-eq-deriv _0 _1 _2 _0 _1 _2 _3 _4)]]]
           [(plus-uniq-deriv-z _0)
            (plus-uniq-deriv
             (z)
             _0
             _0
             (plus-z _0)
             (plus-z _0)
             (plus-eq-deriv-z _0 _0))
            []]
           [(plus-uniq-deriv-s _0 _1 _2 _3 _4 _5 _6)
            (plus-uniq-deriv
             (s _0)
             _1
             (s _2)
             (plus-s _0 _1 _2 _3)
             (plus-s _0 _1 _2 _4)
             (plus-eq-deriv-s _0 _1 _2 _0 _1 _2 _3 _4 _5))
            [[_6 (plus-uniq-deriv _0 _1 _2 _3 _4 _5)]]])))
  (is (= (run 3 [q] (naturals q (nat)))
         '((z) (s (z)) (s (s (z))))))
  (is (= (run* [q] (fresh [n] (naturals q (plus (z) n n))))
         '((plus-z _0))))
  (is (= (run* [q] (fresh [n] (naturals (plus-z n) q)))
         '((plus (z) _0 _0))))
  (is (= (run 3 [q] (fresh [a b c d] (== q [a b c d]) (naturals d (plus a b c))))
         '([(z) _0 _0 (plus-z _0)]
           [(s (z)) _0 (s _0) (plus-s (z) _0 _0 (plus-z _0))]
           [(s (s (z))) _0 (s (s _0)) (plus-s (s (z)) _0 (s _0) (plus-s (z) _0 _0 (plus-z _0)))])))
  (is (= (run* [q] (fresh [a b d n1 n2 n3] (naturals a (plus (z) (z) (z))) (naturals d (sum-inc n1 n2 n3 a b)) (naturals b q)))
         '((plus (z) (s (z)) (s (z))))))
  (is (= (run* [q] (fresh [a b d n n1 n2 n3] (naturals a (plus (z) n n)) (naturals d (sum-inc n1 n2 n3 a b)) (naturals b q)))
         '((plus (z) (s _0) (s _0)))))
  (is (= (run 3 [q] (fresh [a b d n1 n2 n3] (naturals d (sum-inc n1 n2 n3 a b)) (naturals b q)))
         '((plus (z) (s _0) (s _0)) (plus (s (z)) (s (z)) (s (s (z)))) (plus (s (s (z))) (s (z)) (s (s (s (z))))))))
  (is (= (run 1 [q] (fresh [a b c d pa pb n1 n2 n3] (== q [a pa b pb c]) (naturals* [[a pa] [b pb] [d (plus-uniq-deriv n1 n2 n3 a b c)]])))
         '([(plus-z _0) (plus (z) _0 _0) (plus-z _0) (plus (z) _0 _0) (plus-eq-deriv-z _0 _0)])))
  )
