# Version 3.13 (official release)

* Enhancements for plan repair experiments

# Version 3.12

* Use `random-state` library for repeatable experiments.

# Version 3.11

* Command-line applications based on `buildapp`
* HDDL plan output

# Version 3.10

* Add parameter allowing caller to specify number of plans retrieved (not just "one" or "all"), `plan-num-limit`.

# Version 3.9.1

* Add `plan-tree->sexp` for enhanced plan trees (from `find-plans-stack`).

# Version 3.8.1 - [2023/01/24:rpg]

* A large number of minor bug fixes.

* Reorganized the `shop/common/` code subdirectory to clarify the organization of code for state management, particularly to make it easier to introduce new state types.

* Add Shinmera's `random-state` library as a dependency and git submodule. This has not been fully integrated. The intention in bringing it in is to enable reproducdible tests of planning algorithms that involve randomization.

* Add `cl-store` library as a dependency and git submodule.  For now this is also for testing: it enables us to store arbitrary Common Lisp data structures, particularly for use in regression testing.

* Make the `query` function easier to use -- it's a convenient way to use SHOP3's theorem prover, including for non-SHOP3 purposes.

* Make it easier to quash redefinition warnings and other output from creating SHOP3 domains and problems.

* Sometimes we expect that attempts to add primitives (operators or PDDL actions) to the plan will not fail, and that it is a bug if they do. Add the `:operators-dont-fail` slot to SHOP3 domains that can be set to enable checking for this case.

* Add `plan-states` function that can be used to generate a state trajectory corresponding to a plan.
