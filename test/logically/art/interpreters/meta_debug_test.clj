(ns logically.art.interpreters.meta_debug_test
  (:use [logically.art.interpreters.meta_debug] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(deftest test-ex-debug-so-solver-member
  (is (= (run* [q]
           (ex-debug-so-solver-member ['member 'x '(y z)] 2 q))
        '((overflow ((member x (y z)) (member x (z)))))))
  (is (= (run* [q]
           (ex-debug-so-solver-member ['member 'x '(x y z)] 2 q))
        '(no-overflow (overflow ((member x (x y z)) (member x (y z)))))))
  (is (= (run* [q]
           (ex-debug-so-solver-member ['member 'x '(x y z)] 4 q))
        '(no-overflow))))

(deftest text-ex-debug-so-buggy-isort-solver
  (is (= (run* [q]
           (fresh [xs]
             (ex-debug-so-buggy-isort-solver ['isort [2 2] xs] 4 q)
             ;; NOTE: compared to the book, we get (2 2 2 . _0) instead of (2 2 2 2),
             ;;       but this more "general" answer seems to make sense for us,
             ;;       since we'll overflow for any value of _0. Just forcing an answer
             ;;       so that we don't have to check for lcons.
             (== xs [2 2 2 2])))
        '([overflow
            ([isort [2 2] [2 2 2 2]]
             [insert 2 [2] [2 2 2 2]]
             [insert 2 [2] [2 2 2]]
             [insert 2 [2] [2 2]])]))))
