(ns logically.nominal.nsl_test
  (:use [logically.nominal.nsl] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-all-1
  (is (= (run* [q] (ns-typicalo [] q))
        '(([enc [pub bob] [nonce a_0]]
           [enc [pub alice] [pair [nonce a_1] [nonce a_0]]]
           [enc [pub bob] [pair [nonce a_1] [nm alice]]]))))
  (is (= (run* [q] (ns-attacko [] q))
        '(([enc [pub bob] [nonce a_0]]
           [enc [pub eve] [nonce a_0]]
           [enc [pub alice] [pair [nonce a_1] [nonce a_0]]]
           [enc [pub bob] [pair [nonce a_1] [nm alice]]]
           [enc [pub eve] [pair [nonce a_1] [nm alice]]])
          ([enc [pub bob] [pair [nonce a_0] [nm alice]]]
           [enc [pub eve] [nonce a_1]]
           [enc [pub alice] [pair [nonce a_0] [nonce a_1]]]
           [enc [pub bob] [pair [nonce a_0] [nm alice]]]
           [enc [pub eve] [pair [nonce a_0] [nm alice]]]))))
  (is (= (run* [q] (nsl-typicalo [] q))
        '(([enc [pub bob] [nonce a_0]]
           [enc [pub alice] [pair [pair [nonce a_1] [nonce a_0]] [nm bob]]]
           [enc [pub bob] [pair [nonce a_1] [nm alice]]]))))
  (is (= (run* [q] (nsl-attacko [] q))
        '())))
