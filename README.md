# logically

Explorations of logic programming in Clojure's core.logic.

## Sources of inspiration

* [_The Art of Prolog_](http://books.google.ch/books?id=w-XjuvpOrjMC&lpg=PP1&pg=PP1#v=onepage&q&f=false)
  * Chapter 17: Interpreters
     * 17.1: Interpreters for Finite State Machines ([src](src/logically/art/interpreters/fsm.clj)/[test](test/logically/art/interpreters/fsm_test.clj))
     * 17.2: Meta-Interpreters ([src](src/logically/art/interpreters/meta.clj)/[test](test/logically/art/interpreters/meta_test.clj))
     * 17.3: Enhanced Meta-Interpreters for Debugging ([src](src/logically/art/interpreters/meta_debug.clj)/[test](test/logically/art/interpreters/meta_debug_test.clj))

* [_αProlog_](http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/) for [nominal logic programming](http://arxiv.org/abs/cs/0609062)
  * Regular expressions and automata ([src](src/logically/nominal/re.clj)/[test](test/logically/nominal/re_test.clj))

