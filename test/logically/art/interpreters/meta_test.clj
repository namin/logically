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

(deftest test-ex-cf-solver-member
  (is (= (run* [q]
           (ex-cf-solver-member ['member 'b '(a b c)] q))
        '(1)))
  (is (= (run* [q]
           (ex-cf-solver-member ['member 'x '(a b c)] q))
        ()))
  (is (= (run 2 [q]
           (ex-cf-solver-member ['member 'x q] 1))
        [(lcons 'x '_0) (lcons '_0 (lcons 'x '_1))])))

(deftest test-ex-solver-family
  (is (= (run* [q]
           (ex-solver-family ['grandparent q 'diana]))
        '(adam allan aziz)))
  (is (= (run* [q]
           (ex-solver-family ['ancestor 'adam q]))
        '(chris diana emily))))

(deftest test-ex-cf-solver-family
  (is (= (run* [q]
           (fresh [gp c]
             (ex-cf-solver-family ['grandparent gp 'diana] c)
             (== q [gp c])))
        '((adam 0.5) (allan 0.25) (aziz 0.25))))
  (is (= (run* [q]
           (fresh [gp c]
             (ex-cf-solver-family ['grandparent gp 'emily] c)
             (== q [gp c])))
        '((adam 0.05) (allan 0.025) (aziz 0.025))))
  (is (= (run* [q]
           (fresh [x c]
             (ex-cf-solver-family ['ancestor 'adam x] c)
             (== q [x c])))
        '((chris 0.5) (diana 0.5) (emily 0.05)))))

(deftest test-ex-cft-solver-family
  (is (= (run* [q]
           (fresh [gp c]
             (ex-cft-solver-family ['grandparent gp 'diana] c 0)
             (== q [gp c])))
        '((adam 0.5) (allan 0.25) (aziz 0.25))))
  (is (= (run* [q]
           (fresh [gp c]
             (ex-cft-solver-family ['grandparent gp 'emily] c 0)
             (== q [gp c])))
        '((adam 0.05) (allan 0.025) (aziz 0.025))))
  (is (= (run* [q]
           (fresh [d c]
             (ex-cft-solver-family ['ancestor 'sam d] c 0.4)
             (== q [d c])))
        '((adam 0.7) (allan 0.8) (chris 0.35) (diana 0.35)))))
