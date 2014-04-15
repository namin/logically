(ns logically.exp.lf1.quine
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [logically.exp.lf1]))

(defc quine
  [nat       [[] typ]]
  [z         [[] (nat)]]
  [s         [[] (nat) (nat)]]
  [tm [[] typ]]
  [vr [[] (nat) (tm)]]
  [lam [[] (nat) (tm) (tm)]]
  [app [[] (tm) (tm) (tm)]]
  [quo [[] (tm)]]
  [vl [[] typ]]
  [venv [[] typ]]
  [clo [[] (venv) (nat) (tm) (vl)]]
  [code [[] (tm) (vl)]]
  [vnil [[] (venv)]]
  [vcons [[] (nat) (vl) (venv) (venv)]]
  [neq [[] (nat) (nat) typ]]
  [neq-zs [[n#] (neq (z) (s n#))]]
  [neq-sz [[n#] (neq (s n#) (z))]]
  [neq-ss [[n1# n2#] (neq n1# n2#) (neq (s n1#) (s n2#))]]
  [vlookup [[] (venv) (nat) (vl) typ]]
  [vlookup-cons-eq [[x# v# e#] (vlookup (vcons x# v# e#) x# v#)]]
  [vlookup-cons-neq [[x# v# e# y# vy#] (neq x# y#) (vlookup e# x# v#) (vlookup (vcons y# vy# e#) x# v#)]]
  [ev [[] (venv) (tm) (vl) typ]]
  [ev-vr [[e# x# v#] (vlookup e# x# v#) (ev e# (vr x#) v#)]]
  [ev-lam [[e# x# t#] (ev e# (lam x# t#) (clo e# x# t#))]]
  [ev-quo [[e# t#] (ev e# (app (quo) t#) (code t#))]]
  [ev-app-clo [[e# t1# t2# ec# xc# tc# v2# v#] (ev e# t1# (clo ec# xc# tc#)) (ev e# t2# v2#) (ev (vcons xc# v2# ec#) tc# v#) (ev e# (app t1# t2#) v#)]]
  [ev-app-code [[e# t1# t2# c1# c2#] (ev e# t1# (code c1#)) (ev e# t2# (code c2#)) (ev e# (app t1# t2#) (code (app c1# c2#)))]]
)
