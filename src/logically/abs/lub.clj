(ns logically.abs.lub
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.abs.db] :reload))

(defn numbervaro [n f g out-n]
  (conda
   [(all (== f ()) (== g ()) (== out-n n))]
   [(fresh [hf tf hg tg]
           (conso hf tf f)
           (conso hg tg g)
           (conda [(lvaro hf) (== [:var n] hg) (fresh [next-n] (project [n] (== next-n (inc n))) (numbervaro next-n tf tg out-n))]
                  [(fresh [hn]
                          (numbervaro n hf hg hn)
                          (numbervaro hn tf tg out-n))]))]
   [(all (== f g) (== out-n n))]))

(defn set-union [db flag f]
  (fresh [g out-n]
   (numbervaro 0 f g out-n)
   (conda [(db-get-fact db g) fail]
          [succeed])
   (db-add-fact! db f)
   (flag-raise! flag)))
