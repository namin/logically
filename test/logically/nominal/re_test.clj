(ns logically.nominal.re_test
  (:use [logically.nominal.re] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-nfa-1
  (is (= (run* [q]
           (fresh [fa]
             (nom/fresh [a b]
               (nfao a [[a 'a a] [a 'b b] [b 'a a] [b 'b b]] [b] fa)
               (accepto fa (sep "abab")))))
        '(_0)))
  (is (= (run* [q]
           (fresh [fa]
             (nom/fresh [a b]
               (nfao a [[a 'a a] [a 'b b] [b 'a a] [b 'b b]] [b] fa)
               (accepto fa (sep "ababa")))))
        '())))

(deftest test-equiv-1
  (is (= (run* [q]
           (equivo (s 'd) q))
        '([nfa a_0 [[a_0 d a_1]] [a_1]])))
  (is (= (run* [q]
           (equivo (prod (s 'c) (s 'd)) q))
        '([nfa a_0 [[a_1 nil a_2] [a_0 c a_1] [a_2 d a_3]] [a_3]])))
  (is (= (run* [q]
           (equivo (plus (s 'c) (s 'd)) q))
        '([nfa a_0 [[a_0 nil a_1] [a_0 nil a_2] [a_3 nil a_4] [a_5 nil a_4] [a_1 c a_3] [a_2 d a_5]] [a_4]])))
  (is (= (run* [q]
           (equivo (star (s 'c)) q))
        '([nfa a_0 [[a_0 nil a_1] [a_2 nil a_3] [a_0 nil a_3] [a_2 nil a_1] [a_3 c a_2]] [a_1]])))
  (is (= (run* [q]
           (equivo (prod (star (plus (s 'a) (s 'b))) (s 'b)) q))
        '([nfa a_0 [[a_1 nil a_2] [a_0 nil a_1] [a_3 nil a_4] [a_0 nil a_4] [a_3 nil a_1] [a_4 nil a_5] [a_4 nil a_6] [a_7 nil a_3] [a_8 nil a_3] [a_5 a a_7] [a_6 b a_8] [a_2 b a_9]] [a_9]]))))

(deftest test-equiv-accept-1
  (is (= (run* [q]
           (fresh [fa2]
             (equivo (prod (star (plus (s 'a) (s 'b))) (s 'b)) fa2)
             (accepto fa2 (sep "b"))))
        '(_0)))
  (is (= (run* [q]
           (fresh [fa2]
             (equivo (prod (star (plus (s 'a) (s 'b))) (s 'b)) fa2)
             (accepto fa2 (sep "ababa"))))
        '())))
