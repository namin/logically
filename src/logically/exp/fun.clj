(ns logically.exp.fun
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [logically.exp.utils]))

(def empty-env '())

(declare lookup-ext-reco)
(defn lookupo [x env t]
  (conde
   [(fresh [y v rest]
      (== ['ext-env y v rest] env)
      (conde
       [(== y x) (== v t)]
       [(!= y x) (lookupo x rest t)]))]
   [(fresh [defs rest]
      (== ['ext-rec defs rest] env)
      (lookup-ext-reco x defs env rest t))]))

(defn lookup-ext-reco [x defs env rest t]
  (fresh [y lam-exp others]
    (conde
     [(== '() defs) (lookupo x rest t)]
     [(conso [y lam-exp] others defs)
      (conde
       [(== y x) (== ['closure lam-exp env] t)]
       [(!= y x) (lookup-ext-reco x others env rest t)])])))

(declare not-in-defso)
(defn not-in-envo [x env]
  (conde
   [(== empty-env env)]
   [(fresh [y v rest]
      (== ['ext-env y v rest] env)
      (!= y x)
      (not-in-envo x rest))]
   [(fresh [defs rest]
      (== ['ext-rec defs rest] env)
      (not-in-defso x defs)
      (not-in-envo x rest))]))

(defn not-in-defso [x defs]
  (conde
   [(== '() defs)]
   [(fresh [y lam-exp others]
      (conso [y lam-exp] others defs)
      (!= y x)
      (not-in-defso x others))]))

(declare eval-expo)
(defn eval-listo [exp env val]
  (conde
   [(== '() exp)
    (== '() val)]
   [(fresh [a d v-a v-d]
      (conso a d exp)
      (conso v-a v-d val)
      (eval-expo a env v-a)
      (eval-listo d env v-d))]))

(defn list-of-symbolso [los]
  (conde
   [(== '() los)]
   [(fresh [a d]
      (conso a d los)
      (symo a)
      (list-of-symbolso d))]))

(defn listo [ls]
  (conde
   [(== '() ls)]
   [(fresh [a d]
      (conso a d ls)
      (listo d))]))

(defn evalo [exp val]
  (eval-expo exp empty-env val))

(defn ext-envso [xs as env out]
  (conde
   [(== '() xs) (== '() as) (== env out)]
   [(fresh [x a dxs das env2]
      (conso x dxs xs)
      (conso a das as)
      (== ['ext-env x a env] env2)
      (symo x)
      (ext-envso dxs das env2 out))]))

(defn boolean-primo [exp env val]
  (conde
   [(== 'true exp) (== 'true val)]
   [(== 'false exp) (== 'false val)]))

(defn empty?-primo [exp env val]
  (fresh [e v]
    (== `(~'empty? ~e) exp)
    (conde
     [(== '() v) (== 'true val)]
     [(!= '() v) (== 'false val)])
    (not-in-envo 'empty? env)
    (eval-expo e env v)))

(defn symbol?-primo [exp env val]
  (fresh [e v]
    (== `(~'symbol? ~e) exp)
    (conde
     [(symo v) (== 'true val)]
     [(numbero v) (== 'false val)]
     [(fresh [a d]
        (conso a d v)
        (== 'false val))])
    (not-in-envo 'symbol? env)
    (eval-expo e env v)))

(defn not-primo [exp env val]
  (fresh [e b]
    (== `(~'not ~e) exp)
    (conde
     [(!= 'false b) (== 'false val)]
     [(== 'false b) (== 'true val)])
    (not-in-envo 'not env)
    (eval-expo e env b)))

(defn car-primo [exp env val]
  (fresh [p a d]
    (== `(~'car ~p) exp)
    (== a val)
    (!= 'closure a)
    (not-in-envo 'car env)
    (eval-expo p env (lcons a d))))

(defn cdr-primo [exp env val]
  (fresh [p a d]
    (== `(~'cdr ~p) exp)
    (== d val)
    (!= 'closure d)
    (not-in-envo 'cdr env)
    (eval-expo p env (lcons a d))))

(defn cons-primo [exp env val]
  (fresh [a d v-a v-d]
    (== `(~'cons ~a ~d) exp)
    (conso v-a v-d val)
    (not-in-envo 'cons env)
    (eval-expo a env v-a)
    (eval-expo d env v-d)))

(defn =-primo [exp env val]
  (fresh [e1 e2 v1 v2]
    (== `(~'= ~e1 ~e2) exp)
    (conde
     [(== v1 v2) (== 'true val)]
     [(!= v1 v2) (== 'false val)])
    (not-in-envo '= env)
    (eval-expo e1 env v1)
    (eval-expo e2 env v2)))

(defn if-primo [exp env val]
  (fresh [e1 e2 e3 t]
    (== `(~'if ~e1 ~e2 ~e3) exp)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (conde
     [(!= 'false t) (eval-expo e2 env val)]
     [(== 'false t) (eval-expo e3 env val)])))

(defn prim-expo [exp env val]
  (conde
   [(boolean-primo exp env val)]
   [(empty?-primo exp env val)]
   [(symbol?-primo exp env val)]
   [(not-primo exp env val)]
   [(car-primo exp env val)]
   [(cdr-primo exp env val)]
   [(cons-primo exp env val)]
   [(=-primo exp env val)]
   [(if-primo exp env val)]))

(defn eval-expo [exp env val]
  (conde

   [(== `(~'quote ~val) exp)
    (absento 'closure val)
    (not-in-envo 'quote env)]

   [(numbero exp) (== exp val)]

   [(symo exp) (lookupo exp env val)]

   [(fresh [xs body]
      (== `(~'fn ~xs ~body) exp)
      (== `(~'closure (~'fn ~xs ~body) ~env) val)
      (list-of-symbolso xs)
      (not-in-envo 'fn env))]

   [(fresh [as]
      (conso 'list as exp)
      (not-in-envo 'list env)
      (eval-listo as env val))]

   [(fresh [rator xs rands body envh as res]
      (conso rator rands exp)
      (eval-expo rator env `(~'closure (~'fn ~xs ~body) ~envh))
      (eval-listo rands env as)
      (ext-envso xs as envh res)
      (eval-expo body res val))]

   [(fresh [p-name xs body letrec-body]
      (== `(~'letfn [(~p-name ~xs ~body)]
                     ~letrec-body)
          exp)
      (list-of-symbolso xs)
      (not-in-envo 'letrec env)
      (eval-expo letrec-body
                 ['ext-rec [[p-name `(~'fn ~xs ~body)]] env]
                 val))]

   [(prim-expo exp env val)]))
