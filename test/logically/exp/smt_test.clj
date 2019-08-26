(ns logically.exp.smt_test
  (:use [logically.exp.smt] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(deftest unsat-1
  (is (= '()
       (run* [q]
         (smtc `(~'= ~q 1))
         (smtc `(~'= ~q 2))
         (smtc `(~'= ~q 3))))))

(deftest sat-1
  (is (= '(1)
         (run* [q]
           (smtc `(~'= ~q 1))
           smt-purge))))

(deftest sat-2
  (is (= '(1 2)
         (run 2 [q]
           (smtc `(~'> ~q 0))
           smt-purge))))

(deftest sat-conde-1
  (is (= '(1 2)
         (run* [q]
           (conde
            [(smtc `(~'= ~q 1))]
            [(smtc `(~'= ~q 2))])
           smt-purge))))

(deftest mix-constraints
  (is (= '(1)
         (run* [q]
           (predc q number? nil)
           (conde
            [(smtc `(~'= ~q 1))]
            [(== 'hello q)])
           smt-purge))))

(defn faco [n out]
  (conde
   [(smtc `(~'= ~n 0))
    (smtc `(~'= ~out 1))]
   [(smtc `(~'> ~n 0))
    (fresh [n-1 r]
      (smtc `(~'= (~'- ~n 1) ~n-1))
      (smtc `(~'= (~'* ~n ~r) ~out))
      (faco n-1 r))]))

(deftest faco-7
  (is (= '([0 1] [1 1] [2 2] [3 6] [4 24] [5 120] [6 720])
       (run 7 [n out]
         (faco n out)
         smt-purge))))

(deftest faco-backwards-2
  (is (= '(2)
         (run* [q]
           (faco q 2)
           smt-purge))))

(deftest faco-backwards-6
  (is (= '(6)
         (run* [q]
           (faco q 720)
           smt-purge))))

(deftest rsa-small-nums
  (is (= '([143 120 7 703])
         (run 1 [k]
           (fresh [p q n phi e d]
             (smtc `(~'= ~p 11))
             (smtc `(~'= ~q 13))
             (smtc `(~'= ~n (~'* ~p ~q)))
             (smtc `(~'= ~phi (~'* (~'- ~p 1) (~'- ~q 1))))
             (smtc `(~'= ~e 7))
             (smtc `(~'<= 0 ~d))
             (smtc `(~'= (~'mod (~'* ~e ~d) ~phi) 1))
             (== k [n phi e d])
             smt-purge)))))

(deftest bitvec-1
  (is (= '((_0 :- (_0 (_ BitVec 3))))
       (run* [q]
         (smt-decl q '(_ BitVec 3))
         smt-purge))))

(deftest bitvec-2
  (is (= '(bv-001 bv-111 bv-101 bv-011)
       (run* [q]
         (smt-decl q '(_ BitVec 3))
         (smtc `(~'= ~q (~'bvor ~q ~'bv-001)))
         smt-purge))))

(deftest custom-datatypes-1
  (is (= '([(node 21 (as nil (TreeList Int)))
            (node false (as nil (TreeList Bool)))])
       (run 1 [t1 t2]
         (smt-any [t1 t2]
                  '(declare-datatypes
                    (T)
                    ((Tree leaf (node (value T) (children TreeList)))
                     (TreeList nil (cons (car Tree) (cdr TreeList))))))
         (smt-decl t1 '(Tree Int))
         (smt-decl t2 '(Tree Bool))
         (smtc `(~'not (~'= ~t1 (~'as ~'leaf (~'Tree ~'Int)))))
         (smtc `(~'> (~'value ~t1) 20))
         (smtc `(~'not (~'is-leaf ~t2)))
         (smtc `(~'not (~'value ~t2)))
         smt-purge))))
