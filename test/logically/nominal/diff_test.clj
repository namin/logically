(ns logically.nominal.diff_test
  (:use [logically.nominal.diff] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-diffo-1
  (is (= (run* [q]
         (nom/fresh [x]
           (diffo x ['* x x] q)))
      '([+ [* 1 a_0] [* a_0 1]])))
  (is (= (run* [q]
           (nom/fresh [x]
             (diffo x ['integ (nom/tie x x) 0 x] q)))
        '(a_0)))
  (is (= (run* [q]
           (nom/fresh [x y]
             (diffo x ['integ (nom/tie y y) 0 x] q)))
        '(a_0)))
  ;; This is actually 2x according to Wolfram Alpha, but is not covered by our simplified rules.
  ;; http://www.wolframalpha.com/input/?i=d%2Fdx+%28int+x+dy%2C+y%3D0..x%29
  (is (= (run* [q]
           (nom/fresh [x y]
             (diffo x ['integ (nom/tie y x) 0 x] q)))
        '())))

(deftest test-integro-1
  (is (= (run 1 [q]
           (nom/fresh [x]
             (integro x ['cos x] q)))
        '([sin a_0])))
 
  (is (= (run 1 [q]
           (nom/fresh [x]
             (integro x ['sin x] q)))
        '([- [cos a_0]])))
 
  (is (= (run 1 [q]
           (nom/fresh [x]
             (integro x ['+ x x] q)))
        '([* a_0 a_0]))))
