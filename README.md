# A presentation about µKanren

## miniKanren

* design
    - emphasis on *pure relations* and *finite failure* (thesis, p. 7)
    - differences from other logic programming languages
        * "complete (interleaving) search strategy" (compared to Prolog)
        * "full unification" (compared to Mercury, where functions are
            specialized depending on input and output arguments, which
            are annotated as such, disallowing functions with only
            "output" arguments)
        * avoidance of Curry's residuation (suspending certain operations
            until they can operate on non-ground values)
- questions
    - logic + numbers = undecidable. why?
    - `exist` vs `fresh`


## Resources

* [mukanren.scm](./mukanren.scm)
* [µKanren - A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf),
    [original source code](https://github.com/jasonhemann/microKanren)
* Lambda Lounge: Relational Programming in miniKanren by William Byrd:
    - [part 1](https://www.youtube.com/watch?v=zHov3fKYqBA): miniKanren intro
    - [part 2](https://www.youtube.com/watch?v=nFE2E91VDAk): a scheme interpreter (and quines)
* The Reasoned Schemer
* Petite Chez Scheme
* [miniKanren implementation](https://github.com/miniKanren/miniKanren),
    use `(load "<path-to-dir>/mk.scm")` to load it in petite
* <http://minikanren.org>
* [Negation as failure](http://en.wikipedia.org/wiki/Logic_programming#Negation_as_failure)
    - [Negation as failure](https://groups.google.com/forum/#!topic/minikanren/YZsQaQUesWo)
        in `core.logic` ([code](https://github.com/clojure/core.logic/commit/94eab54faa33122f952f19bf2f30364b8723c354)),
        including some notes from Dan Friedman why you often don't want that.
    - William E. Byrd notes in his thesis that you can implement negation as failure in
        miniKanren using `conda` and `condu` (p. 84, footnote #3)
