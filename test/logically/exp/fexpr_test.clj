(ns logically.exp.fexpr_test
  (:use [logically.exp.fexpr] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(comment
  (run 10 [m]
    (termo m))

  (run 10 [m v]
    (semo m v))

  (run 10 [m]
    (valueo m))

  (run 10 [m1 m2]
    (reduceo m1 m2))

  (run 10 [q]
    (nom/fresh [x y]
      (reduceo [['fexpr ['lambda (nom/tie x x)]] ['lambda (nom/tie x x)]] q))))

(deftest test-eval-fexpr-1
  (is (= (run* [q]
           (nom/fresh [x y]
             (reduceo* ['eval [['fexpr ['lambda (nom/tie x x)]] ['lambda (nom/tie x x)]]] q)))
         [['lambda (nom/tie 'a_0 'a_0)]])))
