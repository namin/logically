# logically

Explorations of logic programming in Clojure's core.logic.

## Sources of inspiration

* [_The Art of Prolog_](http://books.google.ch/books?id=w-XjuvpOrjMC&lpg=PP1&pg=PP1#v=onepage&q&f=false)
  * Chapter 17: Interpreters
     * 17.1: Interpreters for Finite State Machines ([src](src/logically/art/interpreters/fsm.clj)/[test](test/logically/art/interpreters/fsm_test.clj))
     * 17.2: Meta-Interpreters ([src](src/logically/art/interpreters/meta.clj)/[test](test/logically/art/interpreters/meta_test.clj))
     * 17.3: Enhanced Meta-Interpreters for Debugging ([src](src/logically/art/interpreters/meta_debug.clj)/[test](test/logically/art/interpreters/meta_debug_test.clj))

<p></p>

* [_Prolog Programming for Artificial Intelligence_](http://www.amazon.com/Programming-Artificial-Intelligence-International-Computer/dp/0321417461)
  * Chapter 25: Meta-Programming
     * 25.6: Explanation-based generalization ([src](src/logically/ai/meta/ebg.clj)/[test](test/logically/ai/meta/ebg_test.clj))

<p></p>

* [_αProlog_](http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/) for [nominal logic programming](http://arxiv.org/abs/cs/0609062)
  * Regular expressions and automata ([src](src/logically/nominal/re.clj)/[test](test/logically/nominal/re_test.clj))
  * Symbolic differentiation ([src](src/logically/nominal/diff.clj)/[test](test/logically/nominal/diff_test.clj))
  * The pi-calculus operational semantics ([src](src/logically/nominal/picalc.clj)/[test](test/logically/nominal/picalc_test.clj))
  * Cryptographic authentication protocols (Needham-Schroeder(-Lowe)) ([src](src/logically/nominal/nsl.clj)/[test](test/logically/nominal/nsl_test.clj))

* [_Meta-Circular Abstract Interpretation_](http://www.cs.bgu.ac.il/~mcodish/Tutorial/)
  * 2 Interpretation and Abstraction ([src](src/logically/abs/meta.clj)/[test](test/logically/abs/meta_test.clj))
  * 3 Concrete Semantics and Interpreters
    * 3.1 Observing the Success Set ([src](src/logically/abs/tp_gr.clj)/[test](test/logically/abs/tp_gr_test.clj))
    * 3.2 Observing answers ([src](src/logically/abs/tp.clj)/[test](test/logically/abs/tp_test.clj))
    * 3.3 Observing calls ([src](src/logically/abs/induced_tp.clj)/[test](test/logically/abs/induced_tp_test.clj))
    * 3.4 Observing program points ([src](src/logically/abs/induced_tp_pp.clj)/[test](test/logically/abs/induced_tp_pp_test.clj))
  * Meta-Circular Abstract Interpretation
    * goal independent analysis ([src](src/logically/abs/tp_abs.clj)/[test](test/logically/abs/tp_abs_test.clj))

