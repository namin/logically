(ns logically.abs.tp_test
  (:use [logically.abs.db] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-db
  (is (= (run* [q]
               (let [db (db-new)]
                 (db-get-fact db q)))
         '()))
  (is (= (run* [q]
               (let [db (db-new)]
                 (all
                  (db-add-fact! db :hello)
                  (db-get-fact db q))))
         '(:hello)))
  (is (= (set (run* [q]
                    (let [db (db-new)]
                      (all
                       (db-add-fact! db :hello)
                       (db-add-fact! db :bye)
                       (db-get-fact db q)))))
         '#{:hello :bye})))
