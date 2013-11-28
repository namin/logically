(ns logically.abs.lub
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db] :reload))

(defn markvaro [f g]
  (conda
   [(all (== f ()) (== g ()))]
   [(fresh [hf tf hg tg x]
           (conso hf tf f)
           (conso hg tg g)
           (conda [(lvaro hf) (== [:var x] hg) (markvaro tf tg)]
                  [(markvaro hf hg)
                   (markvaro tf tg)]))]
   [(all (== f g))]))

(defn set-union [db flag f]
  (fresh [g out-n]
   (markvaro f g)
   (conda [(db-get-fact db g)
           fail]
          [succeed])
   (db-add-fact! db f)
   (flag-raise! flag)))

(defn lub [db flag f]
  (set-union db flag f))
