(ns logically.exp.lf1.naturals
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.exp.lf1]))

(defc naturals
  [nat       [[] typ]]
  [z         [[] (nat)]]
  [s         [[] (nat) (nat)]]
  [plus      [[] (nat) (nat) (nat) typ]]
  [plus-z    [[n#] (plus (z) n# n#)]]
  [plus-s    [[n1# n2# n3#] (plus n1# n2# n3#) (plus (s n1#) n2# (s n3#))]]
  [sum-inc   [[n1# n2# n3#] (plus n1# n2# n3#) (plus n1# (s n2#) (s n3#)) typ]]
  [sum-inc-z [[n#] (sum-inc n# n# n# (plus-z n#) (plus-z (s n#)))]]
  [sum-inc-s [[n1# n2# n3# d1# d2#] (sum-inc n1# n2# n3# d1# d2#) (sum-inc (s n1#) n2# (s n3#) (plus-s n1# n2# n3# d1#) (plus-s n1# (s n2#) (s n3#) d2#))]]
  [plus-eq-deriv [[n1# n2# n3# n4# n5# n6#] (plus n1# n2# n3#) (plus n4# n5# n6#) typ]]
  [plus-eq-deriv-z [[n1# n2#] (plus-eq-deriv (z) n1# n1# (z) n2# n2# (plus-z n1#) (plus-z n2#))]]
  [plus-eq-deriv-s [[n1# n2# n3# n4# n5# n6# p1# p2#] (plus-eq-deriv n1# n2# n3# n4# n5# n6# p1# p2#) (plus-eq-deriv (s n1#) n2# (s n3#) (s n4#) n5# (s n6#) (plus-s n1# n2# n3# p1#) (plus-s n4# n5# n6# p2#))]]
  [plus-uniq-deriv [[n1# n2# n3#] [p1# (plus n1# n2# n3#)] [p2# (plus n1# n2# n3#)] (plus-eq-deriv n1# n2# n3# n1# n2# n3# p1# p2#) typ]]
  [plus-uniq-deriv-z [[n#] (plus-uniq-deriv (z) n# n# (plus-z n#) (plus-z n#) (plus-eq-deriv-z n# n#))]]
  [plus-uniq-deriv-s [[n1# n2# n3# p1# p2# s#] (plus-uniq-deriv n1# n2# n3# p1# p2# s#) (plus-uniq-deriv (s n1#) n2# (s n3#) (plus-s n1# n2# n3# p1#) (plus-s n1# n2# n3# p2#) (plus-eq-deriv-s n1# n2# n3# n1# n2# n3# p1# p2# s#))]])
