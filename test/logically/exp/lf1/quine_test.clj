(ns logically.exp.lf1.quine_test
  (:use [logically.exp.lf1.quine])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(defn simple-quine []
  (app (lam (z) (app (vr (z)) (app (app (quo) (quo)) (vr (z)))))
       (app (quo) (lam (z) (app (vr (z)) (app (app (quo) (quo)) (vr (z))))))))

(deftest test-quine
  (is (quine-ok))
  (is (= (run* [q] (quine (simple-quine) (tm)))
         '(_0)))
  (is (= (run* [q] (fresh [e] (quine e (ev (vnil) (simple-quine) (code (simple-quine))))))
         '(_0)))
  (is (= (run 1 [q] (fresh [e] (quine e (ev (vnil) q (code q)))))
         '((app
            (lam _0 (app (vr _0) (app (app (quo) (quo)) (vr _0))))
            (app (quo) (lam _0 (app (vr _0) (app (app (quo) (quo)) (vr _0))))))))))
