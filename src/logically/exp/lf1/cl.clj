(ns logically.exp.lf1.cl
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.exp.lf1]))

(defc lc
  [exp [[] typ]]
  [s [[] (exp)]]
  [k [[] (exp)]]
  [app [[] (exp) (exp) (exp)]]
  [contracto [[] (exp) (exp) typ]]
  [contracto-k [[u# y#] (contracto (app (app (k) u#) y#) u#)]]
  [contracto-s [[x# y# z#] (contracto (app (app (app (s) x#) y#) z#) (app (app x# z#) (app y# z#)))]]
  [wo1 [[] (exp) (exp) typ]]
  [wo1-contracto [[u# v#] (contracto u# v#) (wo1 u# v#)]]
  [wo1-1 [[v1# v2# v#] (wo1 v1# v#) (wo1 (app v1# v2#) (app v# v2#))]]
  [wo1-2 [[v1# v2# v#] (wo1 v2# v#) (wo1 (app v1# v2#) (app v1# v#))]]
  [wo [[] (exp) (exp) typ]]
  [wo-id [[u#] (wo u# u#)]]
  [wo-step [[u# v# w#] (wo1 u# w#) (wo w# v#) (wo u# v#)]])

