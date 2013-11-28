(ns logically.abs.meta_test
  (:use [logically.abs.meta] :reload)
  (:use [logically.abs.unif] :reload)
  (:use [logically.abs.ex_ack] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

;; confirms http://www.cs.bgu.ac.il/~mcodish/Tutorial/meta.html
(deftest test-ack
  (is (= (run 3 [q]
              (fresh [m n]
                     (== q [m n])
                     (solve* u==-concrete ack-norm-clause [[:ack [:s [:s 0]] n [:s m]]])))
         '([[:s [:s 0]] 0]
           [[:s [:s [:s [:s 0]]]] [:s 0]]
           [[:s [:s [:s [:s [:s [:s 0]]]]]] [:s [:s 0]]])))
  (is (= (run 3 [q]
              (solve* u==-parity1 ack-norm-clause [[:ack :even :one q]]))
         '(:odd :odd :odd))))


