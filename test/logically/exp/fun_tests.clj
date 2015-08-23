(ns logically.exp.fun_tests
  (:use [logically.exp.fun] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [clojure.test]))

(deftest eval-expo-1
  (is (= (run* [q] (eval-expo '5 '() q))
         '(5))))

(deftest eval-expo-2
  (is (= (run* [q] (eval-expo 'x '() q))
         '())))

(deftest eval-expo-3
  (is (= (run* [q] (eval-expo '(fn [x] x) '() q))
         '((closure (fn [x] x) ())))))

(deftest eval-expo-4
  (is (= (run* [q] (eval-expo '((fn [x] x) 5) '() q))
         '(5))))

(deftest append-1
  (is (= (run* [q]
           (evalo
            '(letfn [(append [l s]
                       (if (empty? l)
                         s
                         (cons (car l) (append (cdr l) s))))]
               (append '(a b c) '(d e)))
            q))
         '((a b c d e)))))

(deftest append-2
  (is (= (run* [x y]
           (evalo
            `(~'letfn [(~'append [~'l ~'s]
                       (~'if (~'empty? ~'l)
                         ~'s
                         (~'cons (~'car ~'l) (~'append (~'cdr ~'l) ~'s))))]
               (~'append (~'quote ~x) (~'quote ~y)))
            '(a b c d e)))
         '((() (a b c d e))
           ((a) (b c d e))
           ((a b) (c d e))
           ((a b c) (d e))
           ((a b c d) (e))
           ((a b c d e) ())))))
