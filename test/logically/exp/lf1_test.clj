(ns logically.exp.lf1_test
  (:use [logically.exp.lf1] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-naturals
  (is (= (run* [q] (fresh [a b c] (== q [c a b]) (naturals-clauses c a b) (== a typ)))
         '([nat typ []]
           [(plus _0 _1 _2) typ [[_0 nat] [_1 nat] [_2 nat]]]
           [(sum-inc _0 _1)
            typ
            [[_0 (plus _2 _3 _4)] [_1 (plus _2 (s _3) (s _4))]]]
           [(plus-eq-deriv _0 _1)
            typ
            [[_0 (plus _2 _3 _4)] [_1 (plus _5 _6 _7)]]]
           [(plus-uniq-deriv _0 _1 _2)
            typ
            [[_0 (plus _3 _4 _5)]
             [_1 (plus _3 _4 _5)]
             [_2 (plus-eq-deriv _0 _1)]]])))
  (is (= (run* [q] (fresh [a b c] (== q [c a b]) (naturals-clauses c a b)))
         '([nat typ []]
           [z nat []]
           [(s _0) nat [[_0 nat]]]
           [(plus _0 _1 _2) typ [[_0 nat] [_1 nat] [_2 nat]]]
           [plus-z (plus z _0 _0) []]
           [(plus-s _0) (plus (s _1) _2 (s _3)) [[_0 (plus _1 _2 _3)]]]
           [(sum-inc _0 _1)
            typ
            [[_0 (plus _2 _3 _4)] [_1 (plus _2 (s _3) (s _4))]]]
           [sum-inc-z (sum-inc plus-z plus-z) []]
           [(sum-inc-s _0)
            (sum-inc (plus-s _1) (plus-s _2))
            [[_0 (sum-inc _1 _2)]]]
           [(plus-eq-deriv _0 _1)
            typ
            [[_0 (plus _2 _3 _4)] [_1 (plus _5 _6 _7)]]]
           [plus-eq-deriv-z (plus-eq-deriv plus-z plus-z) []]
           [(plus-eq-deriv-s _0)
            (plus-eq-deriv (plus-s _1) (plus-s _2))
            [[_0 (plus-eq-deriv _1 _2)]]]
           [(plus-uniq-deriv _0 _1 _2)
            typ
            [[_0 (plus _3 _4 _5)]
             [_1 (plus _3 _4 _5)]
             [_2 (plus-eq-deriv _0 _1)]]]
           [plus-uniq-deriv-z (plus-uniq-deriv plus-z plus-z plus-eq-deriv-z) []]
           [(plus-uniq-deriv-s _0)
            (plus-uniq-deriv (plus-s _1) (plus-s _2) (plus-eq-deriv-s _3))
            [[_0 (plus-uniq-deriv _1 _2 _3)]]])))
  (is (= (run 3 [q] (naturals q nat))
         '(z (s z) (s (s z)))))
  (is (= (run* [q] (fresh [n] (naturals q (plus z n n))))
         '(plus-z)))
  (is (= (run* [q] (naturals plus-z q))
         '((plus z _0 _0))))
  (is (= (run 3 [q] (fresh [a b c d] (== q [a b c d]) (naturals d (plus a b c))))
         '([z _0 _0 plus-z]
           [(s z) _0 (s _0) (plus-s plus-z)]
           [(s (s z)) _0 (s (s _0)) (plus-s (plus-s plus-z))])))
  (is (= (run* [q] (fresh [a b d] (naturals a (plus z z z)) (naturals d (sum-inc a b)) (naturals b q)))
         '((plus z _0 _0))))
  (is (= (run 3 [q] (fresh [a b d] (naturals d (sum-inc a b)) (naturals b q)))
         '((plus z _0 _0) (plus (s z) _0 (s _0)) (plus (s (s z)) _0 (s (s _0))))))
  (is (= (run 1 [q] (fresh [a b c d pa pb] (== q [a pa b pb c]) (naturals* [[a pa] [b pb] [d (plus-uniq-deriv a b c)]])))
         ;; TODO: This is arguably not what we want!
         ;;       plus-uniq-deriv should enforce _0 == _1.
         '([plus-z (plus z _0 _0) plus-z (plus z _1 _1) plus-eq-deriv-z])))
  )
