(ns logically.nominal.picalc_test
  (:use [logically.nominal.picalc] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-step-1
  (is (= (run* [r]
           (fresh [p action q]
             (nom/fresh [a b c]
               (== p `(~'res ~(nom/tie a `(~'par (~'out ~c ~a ~'ina) (~'in ~c ~(nom/tie b `(~'out ~b ~c ~'ina)))))))
               (stepo p action q)
               (== r [action q]))))
        `([~'tau_a (~'res ~(nom/tie 'a_0 '(par ina (out a_0 a_1 ina))))])))

  (is (= (run* [r]
           (fresh [p action q]
             (nom/fresh [a b c]
               (== p `(~'par (~'out ~c ~a ~'ina) (~'in ~c ~(nom/tie b `(~'out ~b ~c ~'ina)))))
               (stepo p action q)
               (== r [action q]))))
        [['(out_a a_0 a_1) `(~'par ~'ina (~'in ~'a_0 ~(nom/tie 'a_2 '(out a_2 a_0 ina))))]
         ['tau_a '(par ina (out a_0 a_1 ina))]]))
  (is (= (run* [r]
           (fresh [p action q]
             (nom/fresh [a b c]
               (== p `(~'res ~(nom/tie b `(~'in ~a ~(nom/tie b `(~'out ~a ~b ~'ina))))))
               (stepo p action q)
               (== r [action q]))))
        '()))
  (is (= (run* [r]
           (fresh [p action q]
             (nom/fresh [a x y]
               (== p `(~'par (~'res ~(nom/tie x `(~'out ~a ~x (~'in ~x ~(nom/tie y 'ina))))) (~'in ~a ~(nom/tie x `(~'out ~x ~a ~'ina)))))
               (stepo p action q)
               (== r [action q]))))
        `([~'tau_a (~'res ~(nom/tie 'a_0 `(~'par (~'res ~(nom/tie 'a_1 `(~'in ~'a_0 ~(nom/tie 'a_2 'ina)))) (~'out ~'a_0 ~'a_3 ~'ina))))])))
  (is (= (run* [r]
           (fresh [p action q]
             (nom/fresh [a b c]
               (== p `(~'res ~(nom/tie a `(~'par (~'out ~c ~a ~'ina) (~'in ~c ~(nom/tie b `(~'out ~b ~c ~'ina)))))))
               (stepo p action q)
               (== r [action q]))))
        `([~'tau_a (~'res ~(nom/tie 'a_0 '(par ina (out a_0 a_1 ina))))])))
  (is (= (run* [r]
           (fresh [p action q]
             (nom/fresh [a b c]
               (== p `(~'res ~(nom/tie b `(~'in ~b ~(nom/tie a `(~'out ~a ~b ~'ina))))))
               (stepo p action q)
               (== r [action q]))))
        '())))
