(ns logically.art.interpreters.meta_debug
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l])
  (:use [logically.art.interpreters.meta]))

;; Section 17.3 Enhanced Meta-Interpreters for Debugging

;; Program 17.11 A meta-interpreter detecting a stack overflow
(defn debug-so-solve-for [clause]
  (letfn [(solve0 [goal depth overflow]
            (solve [goal] depth overflow))
          (solve [goals d o]
            (conde
              [(== goals ())
               (== o 'no-overflow)]
              [(!= goals ())
               (== d 0)
               (== o ['overflow ()])]
              [(fresh [g gs b db og ob]
                 (project [d]
                   (== (> d 0) true)
                   (== db (dec d)))
                 (conso g gs goals)
                 (clause g b)
                 (solve b db ob)
                 (return-overflow ob g og)
                 (solve-conjunction og gs d o))]))
          (solve-conjunction [og gs d o]
            (conde
              [(fresh [s]
                 (== og ['overflow s])
                 (== o ['overflow s]))]
              [(== og 'no-overflow)
               (solve gs d o)]))
          (return-overflow [ob g og]
            (conde
              [(== ob 'no-overflow)
               (== og 'no-overflow)]
              [(fresh [s]
                 (== ob ['overflow s])
                 (== og ['overflow (lcons g s)]))]))]
    solve0))

(def ex-debug-so-solver-member (debug-so-solve-for solver-member-clause))

;; Program 17.12 A non-terminating insertion sort
(defn solver-so-buggy-isort-clause [a b]
  (conde
    [(fresh [xs ys]
       (== a ['isort xs ys])
       (conde
         [(== xs ())
          (== ys ())
          (== b ())]
         [(fresh [x xt zs]
            (conso x xt xs)
            (== b [['isort xt zs] ['insert x zs ys]]))]))]
    [(fresh [x s r]
       (== a ['insert x s r])
       (conde
         [(fresh [y ys]
            (conso y ys s)
            (conde
              [(conso x s r)
               (project [x y]
                 (== (< x y) true))
               (== b ())]
              [(fresh [zs]
                 (conso y zs r)
                 (project [x y]
                   (== (>= x y) true))
                 (== b [['insert y (lcons x ys) zs]]))]))]
         [(== s ())
          (== r [x])
          (== b ())]))]))

(def ex-debug-so-buggy-isort-solver (debug-so-solve-for solver-so-buggy-isort-clause))

;; Program 17.13 An incorrect and incomplete insertion sort
(defn solver-buggy-isort-clause [a b]
  (conde
    [(fresh [xs ys]
       (== a ['isort xs ys])
       (conde
         [(== xs ())
          (== ys ())
          (== b ())]
         [(fresh [x xt zs]
            (conso x xt xs)
            (== b [['isort xt zs] ['insert x zs ys]]))]))]
    [(fresh [x s r]
       (== a ['insert x s r])
       (conde
         [(fresh [y ys]
            (conso y ys s)
            (conde
              [(conso x s r)
               (project [x y]
                 (== (>= x y) true))
               (== b ())]
              [(fresh [zs]
                 (conso y zs r)
                 (project [x y]
                   (== (> x y) true))
                 (== b [['insert x ys zs]]))]))]
         [(== s ())
          (== r [x])
          (== b ())]))]))

(def ex-proof-false-buggy-isort-solver (solve-proof-for solver-buggy-isort-clause))

;; Program 17.14 Bottom-up diagnosis of a false solution
(defn extract-body [pb b1]
  (conde
    [(== pb ()) (== b1 ())]
    [(fresh [a pr r]
       (conso [a '<-- pr] pr pb)
       (conso a r b1)
       (extract-body pr r))]))

(defn debug-false-solution-for [clause oracle]
  (let [solve (solve-proof-for clause)]
    (letfn [(false-solution [a c]
              (fresh [proof]
                (solve a proof)
                (false-clause proof c)))
            (false-clause [proof c]
              (conde
                [(== proof ())
                 (== c 'ok)]
                [(fresh [a pb ps ca cb]
                   (conso [a '<-- pb] ps proof)
                   (false-clause pb cb)
                   (check-clause cb a pb ca)
                   (check-conjunction ca ps c))]))
            (check-conjunction [ca ps c]
              (conde
                [(== ca 'ok)
                 (false-clause ps c)]
                [(fresh [a b1]
                   (== ca [a '<-- b1])
                   (== ca c))]))
            (check-clause [cb a pb ca]
              (conde
                [(== cb 'ok)
                 (fresh [answer]
                   (oracle a answer)
                   (check-answer answer a pb ca))]
                [(fresh [b b1]
                   (== cb [b '<-- b1])
                   (== ca cb))]))
            (check-answer [answer a pb ca]
              (conde
                [(== answer true)
                 (== ca 'ok)]
                [(== answer false)
                 (fresh [b1]
                   (== ca [a '<-- b1])
                   (extract-body pb b1))]))]
      false-solution)))

(defn interactive-oracle [goal answer]
  (fn [a]
    (println "Is the goal " (walk* a goal) " true?")
    (let [r (let [user-answer (read-line)]
              (if (or (.startsWith user-answer "y") (.startsWith user-answer "t"))
                true
                false))]
      (println "Assuming " r)
      (bind a (== answer r)))))

(def ex-debug-false-buggy-isort-interactive (debug-false-solution-for solver-buggy-isort-clause interactive-oracle))

(comment
  (doall
    (run 1 [q]
      (fresh [xs]
        (ex-debug-false-buggy-isort-interactive ['isort [3 2 1] xs] q)))))

(defn solver-isort-clause [a b]
  (conde
    [(fresh [xs ys]
       (== a ['isort xs ys])
       (conde
         [(== xs ())
          (== ys ())
          (== b ())]
         [(fresh [x xt zs]
            (conso x xt xs)
            (== b [['isort xt zs] ['insert x zs ys]]))]))]
    [(fresh [x s r]
       (== a ['insert x s r])
       (conde
         [(fresh [y ys]
            (conso y ys s)
            (conde
              [(conso x s r)
               (project [x y]
                 (== (<= x y) true))
               (== b ())]
              [(fresh [zs]
                 (conso y zs r)
                 (project [x y]
                   (== (> x y) true))
                 (== b [['insert x ys zs]]))]))]
         [(== s ())
          (== r [x])
          (== b ())]))]))

(def ex-solver-isort (solve-for solver-isort-clause))

(defn make-oracle
  ([clause] (make-oracle clause nil))
  ([clause log]
   (let [solver (solve-for clause)]
     (fn [goal answer]
       (fn [a]
         (let [g (walk* a goal)]
           (let [r (not (empty? (run 1 [q] (solver g))))]
             (when log (log g r))
             (bind a (== answer r)))))))))

(def ex-debug-false-buggy-isort-print
  (debug-false-solution-for solver-buggy-isort-clause
    (make-oracle solver-isort-clause (fn [g r] (println "Assuming" g r)))))
(def ex-debug-false-buggy-isort
  (debug-false-solution-for solver-buggy-isort-clause (make-oracle solver-isort-clause)))

(comment
  (doall
    (run 1 [q]
      (fresh [xs]
        (ex-debug-false-buggy-isort-print ['isort [3 2 1] xs] q)))))

(defn make-subst-oracle
  ([clause] (make-subst-oracle clause nil))
  ([clause log]
   (let [solver* (solve-for* clause)]
     (fn [a b]
       (fn [s]
         (bind*
           s
           (onceo (clause a b))
           (onceo (solver* b))
           (fn [sp]
             (when log (log (-reify s (walk* s [a '<-- b])) [(walk* sp [a '<-- b])]))
             sp)))))))

;; Program 17.16 Diagnosing missing solution
(defn debug-missing-solution-for [clause subst-oracle]
  (letfn [(missing-solution0 [goal missing]
            (missing-solution [goal] missing))
          (missing-solution [goals missing]
            (fresh [a b gs]
              (conso a gs goals)
              (conda
                [(clause a b)
                 (fresh [r]
                   (subst-oracle a b)
                   (appendo b gs r)
                   (missing-solution r missing))]
                [(subst-oracle a b)
                 (== a missing)])))]
    missing-solution0))

(def ex-debug-missing-buggy-isort-print
  (debug-missing-solution-for solver-buggy-isort-clause
    (make-subst-oracle solver-isort-clause
      (fn [before after] (println "before:" before "\nafter: " after "\n")))))

(def ex-debug-missing-buggy-isort
  (debug-missing-solution-for solver-buggy-isort-clause (make-subst-oracle solver-isort-clause)))

(comment
  (doall
    (run* [q]
      (ex-debug-missing-buggy-isort-print ['isort [2 1 3] [1 2 3]] q))))
