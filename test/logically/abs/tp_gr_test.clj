(ns logically.abs.tp_gr_test
  (:use [logically.abs.tp_gr] :reload)
  (:use [logically.abs.ex_path] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-impl
  (is (= (run* [q] (ground-term path-clause path-symbol q))
         '(:a :b :c)))
  (is (= (run* [q] (fresh [a b] (== q [:edge a b]) (ground-instance path-clause path-symbol [:edge a b])))
         '([:edge :a :a]
           [:edge :a :b]
           [:edge :b :a]
           [:edge :a :c]
           [:edge :c :a]
           [:edge :b :b]
           [:edge :c :b]
           [:edge :b :c]
           [:edge :c :c])))
  (is (= (run* [q] (tp path-clause path-symbol () q))
         '([:edge :a :b] [:edge :b :a] [:edge :a :c])))
  (is (= (run* [q] (deriveo path-clause path-symbol [] q))
         '(([:edge :a :b] [:edge :b :a] [:edge :a :c]))))
  (is (= (run* [q] (fresh [l]
                          (== l [])
                          (deriveo path-clause path-symbol l q)))
         '(([:edge :a :b] [:edge :b :a] [:edge :a :c])))))

;; confirms http://www.cs.bgu.ac.il/~mcodish/Tutorial/tp_gr.html
(deftest test-path
  (is (= (run* [q] (iterate-tp path-clause path-symbol () [:start] q))
        '(([:path :a :a]
           [:path :b :c]
           [:path :b :b]
           [:path :a :b]
           [:path :b :a]
           [:path :a :c]
           [:edge :a :c]
           [:edge :b :a]
           [:edge :a :b])))))


