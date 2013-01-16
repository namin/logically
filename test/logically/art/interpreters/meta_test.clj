(ns logically.art.interpreters.meta_test
  (:use [logically.art.interpreters.meta] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-ex-solver-member
  (is (= (run* [q]
           (ex-solver-member ['member 'x '(y)]))
        ()))
  (is (= (run 2 [q]
           (ex-solver-member ['member 'x q]))
        [(lcons 'x '_0) (lcons '_0 (lcons 'x '_1))])))

(deftest test-ex-alt-solver-member
  (is (= (run* [q]
           (ex-alt-solver-member ['member 'x '(y)]))
        ()))
  (is (= (run 2 [q]
           (ex-alt-solver-member ['member 'x q]))
        [(lcons 'x '_0) (lcons '_0 (lcons 'x '_1))])))

(deftest test-ex-solver-trace-member
  (is (re-matches
         #" \[member a \(a b c\)\]
 \[member <lvar:\w+> \(a b c\)\]
    \[member b \(b c\)\]
    \[member <lvar:\w+> \(b c\)\]
       \[member c \(c\)\]
       \[member <lvar:\w+> \(c\)\]
"
        (with-out-str
          (doall
            (run* [q]
              (ex-trace-solver-member ['member q '(a b c)])))))))

(deftest test-ex-solver-proof-member
  (is (= (run* [q]
           (ex-proof-solver-member ['member 'b '(a b c)] q))
        '(((member b (a b c)) <-- ((member b (b c)) <-- ()))))))
