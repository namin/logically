(ns logically.exp.utils
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

;;; prettier reification for single-variable constraints
(defn reifier-for [tag x]
  (fn [c v r a]
    (let [x (walk* r (walk* a x))]
      (when (symbol? x)
        `(~tag ~x)))))

(defn symo [x] (predc x symbol? (reifier-for 'sym x)))
