(ns logically.art.interpreters.meta_layered_test
  (:use [logically.art.interpreters.meta] :reload)
  (:use [logically.art.interpreters.meta_layered] :reload)
  (:use [logically.exp.prop])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(defn boolo [q]
  (conde
   [(== q true)]
   [(== q false)]))

(deftest test-wip
  (is (= (run* [out]
             (fresh [a b c proof ok]
                    (== out [a proof ok])
                    (solve-top solver-member-clause ['member 'x [a]] proof ok)))

'([x ([[member x [x]] :<-- ()]) true]
 ([_0
   [:not [member x [_0]] :<-- [:assume [member x [_0]] false]]
   false]
  :-
  (!= (_0 x)))
 ([_0
   [:not
    [member x [_0]]
    :<--
    [:not [member x ()] :<-- [:assume [member x ()] false]]]
   false]
  :-
  (!= (_0 x))))

         ))

  (is (= (run* [out]
               (fresh [proof ok]
                      (== out [proof ok])
                      (solve-top solver-member-clause ['member 'x '(x x y x)] proof ok)))

'([([[member x (x x y x)] :<-- ()]) true]
 [([[member x (x x y x)] :<-- ([[member x (x y x)] :<-- ()])]) true]
 [([[member x (x x y x)]
    :<--
    ([[member x (x y x)]
      :<--
      ([[member x (y x)] :<-- ([[member x (x)] :<-- ()])])])])
  true])

         ))

  (is (= (run* [out]
               (fresh [proof ok]
                      (== out [proof ok])
                      (solve-top solver-member-clause ['member 'x '(a b c)] proof ok)))

'([[:not [member x (a b c)] :<-- [:assume [member x (a b c)] false]]
  false]
 [[:not
   [member x (a b c)]
   :<--
   [:not [member x (b c)] :<-- [:assume [member x (b c)] false]]]
  false]
 [[:not
   [member x (a b c)]
   :<--
   [:not
    [member x (b c)]
    :<--
    [:not [member x (c)] :<-- [:assume [member x (c)] false]]]]
  false]
 [[:not
   [member x (a b c)]
   :<--
   [:not
    [member x (b c)]
    :<--
    [:not
     [member x (c)]
     :<--
     [:not [member x ()] :<-- [:assume [member x ()] false]]]]]
  false])

         ))

  (is (= (run* [out]
               (fresh [model pv qv proof ok]
                      (boolo pv)
                      (boolo qv)
                      (== out [pv qv proof ok])
                      (== model [['p pv] ['q qv]])
                      (solve-top prop-clause [:evalo [:implies [:and 'p [:implies 'p 'q]] 'q] true model] proof ok)))

'([false
  true
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p false] [q true]]]
    :<--
    ([[:evalo [:and p [:implies p q]] false [[p false] [q true]]]
      :<--
      ([[:evalo p false [[p false] [q true]]] :<-- ()])])])
  true]
 [false
  false
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p false] [q false]]]
    :<--
    ([[:evalo [:and p [:implies p q]] false [[p false] [q false]]]
      :<--
      ([[:evalo p false [[p false] [q false]]] :<-- ()])])])
  true]
 [true
  true
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p true] [q true]]]
    :<--
    ([[:evalo [:and p [:implies p q]] true [[p true] [q true]]]
      :<--
      ([[:evalo p true [[p true] [q true]]] :<-- ()]
       [[:evalo [:implies p q] true [[p true] [q true]]]
        :<--
        ([[:evalo p true [[p true] [q true]]] :<-- ()]
         [[:evalo q true [[p true] [q true]]] :<-- ()])])]
     [[:evalo q true [[p true] [q true]]] :<-- ()])])
  true]
 [true
  false
  ([[:evalo
     [:implies [:and p [:implies p q]] q]
     true
     [[p true] [q false]]]
    :<--
    ([[:evalo [:and p [:implies p q]] false [[p true] [q false]]]
      :<--
      ([[:evalo p true [[p true] [q false]]] :<-- ()]
       [[:evalo [:implies p q] false [[p true] [q false]]]
        :<--
        ([[:evalo p true [[p true] [q false]]] :<-- ()]
         [[:evalo q false [[p true] [q false]]] :<-- ()])])])])
  true])

         ))

  (is (= (run* [out]
               (fresh [model pv qv proof ok]
                      (== out [pv qv proof ok])
                      (== model [['p pv] ['q qv]])
                      (solve-top prop-clause [:evalo [:implies [:and 'p [:implies 'p 'q]] 'q] false model] proof ok)))

'([_0
  _1
  [:not
   [:evalo [:implies [:and p [:implies p q]] q] false [[p _0] [q _1]]]
   :<--
   [:assume
    [:evalo [:implies [:and p [:implies p q]] q] false [[p _0] [q _1]]]
    false]]
  false]
 ([_0
   _1
   [:not
    [:evalo [:implies [:and p [:implies p q]] q] false [[p _0] [q _1]]]
    :<--
    [:not
     [:evalo [:and p [:implies p q]] true [[p _0] [q _1]]]
     :<--
     [:assume
      [:evalo [:and p [:implies p q]] true [[p _0] [q _1]]]
      false]]]
   false]
  :-
  (!= (_1 true) (_0 true)))
 ([_0
   _1
   [:not
    [:evalo [:implies [:and p [:implies p q]] q] false [[p _0] [q _1]]]
    :<--
    [:not
     [:evalo [:and p [:implies p q]] true [[p _0] [q _1]]]
     :<--
     [:not
      [:evalo p true [[p _0] [q _1]]]
      :<--
      [:assume [:evalo p true [[p _0] [q _1]]] false]]]]
   false]
  :-
  (!= (_1 _2) (_0 true))
  (!= (_1 true) (_0 true)))
 ([true
   _0
   [:not
    [:evalo
     [:implies [:and p [:implies p q]] q]
     false
     [[p true] [q _0]]]
    :<--
    [:not
     [:evalo [:and p [:implies p q]] true [[p true] [q _0]]]
     :<--
     ([[:evalo p true [[p true] [q _0]]] :<-- ()]
      :not
      [:evalo [:implies p q] true [[p true] [q _0]]]
      :<--
      [:assume [:evalo [:implies p q] true [[p true] [q _0]]] false])]]
   false]
  :-
  (!= (_0 true)))
 ([true
   _0
   [:not
    [:evalo
     [:implies [:and p [:implies p q]] q]
     false
     [[p true] [q _0]]]
    :<--
    [:not
     [:evalo [:and p [:implies p q]] true [[p true] [q _0]]]
     :<--
     ([[:evalo p true [[p true] [q _0]]] :<-- ()]
      :not
      [:evalo [:implies p q] true [[p true] [q _0]]]
      :<--
      [:not
       [:evalo p true [[p true] [q _0]]]
       :<--
       [:assume [:evalo p true [[p true] [q _0]]] false]])]]
   false]
  :-
  (!= (_0 true))
  (!= (_0 _1)))
 ([true
   _0
   [:not
    [:evalo
     [:implies [:and p [:implies p q]] q]
     false
     [[p true] [q _0]]]
    :<--
    [:not
     [:evalo [:and p [:implies p q]] true [[p true] [q _0]]]
     :<--
     ([[:evalo p true [[p true] [q _0]]] :<-- ()]
      :not
      [:evalo [:implies p q] true [[p true] [q _0]]]
      :<--
      [:not
       [:evalo p false [[p true] [q _0]]]
       :<--
       [:assume [:evalo p false [[p true] [q _0]]] false]])]]
   false]
  :-
  (!= (_0 true)))
 [true
  true
  [:not
   [:evalo
    [:implies [:and p [:implies p q]] q]
    false
    [[p true] [q true]]]
   :<--
   ([[:evalo [:and p [:implies p q]] true [[p true] [q true]]]
     :<--
     ([[:evalo p true [[p true] [q true]]] :<-- ()]
      [[:evalo [:implies p q] true [[p true] [q true]]]
       :<--
       ([[:evalo p true [[p true] [q true]]] :<-- ()]
        [[:evalo q true [[p true] [q true]]] :<-- ()])])]
    :not
    [:evalo q false [[p true] [q true]]]
    :<--
    [:assume [:evalo q false [[p true] [q true]]] false])]
  false]
 ([true
   _0
   [:not
    [:evalo
     [:implies [:and p [:implies p q]] q]
     false
     [[p true] [q _0]]]
    :<--
    [:not
     [:evalo [:and p [:implies p q]] true [[p true] [q _0]]]
     :<--
     ([[:evalo p true [[p true] [q _0]]] :<-- ()]
      :not
      [:evalo [:implies p q] true [[p true] [q _0]]]
      :<--
      ([[:evalo p true [[p true] [q _0]]] :<-- ()]
       :not
       [:evalo q true [[p true] [q _0]]]
       :<--
       [:assume [:evalo q true [[p true] [q _0]]] false]))]]
   false]
  :-
  (!= (_0 true))))

         ))
)


