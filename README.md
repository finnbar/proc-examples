# proc-examples

This repo contains a collection of small code snippets written using Haskell's proc notation. They are designed to test awkward cases in the desugaring, and each come with an explanation of the expected results of each program along with the edge case it is testing. We provide the following examples:

* `Choice.hs` contains examples which desugar to the choice operator - so `if` and `case` expressions. There are two simple tests designed to fail (for the error messages of if/case when the branch types do not match) and one designed to succeed (for nested case, which desugars to nested Either values).
* `ProcRec.hs` contains examples which desugar to `loop`. We have a test with a large `rec` statement (which will require a lot of routing in its desugared version), a pair of small tests that test shadowing within `rec` (the rules seem to be that definitions within a `rec` override those outside of that `rec`) and one for nested `rec` (which requires a lot of routing).
* `Shadowing.hs` tests more examples related to variables being shadowed. We have two tests: `nestedProc` has a proc defined within a proc via a `let`, which contains some variables defined in the outer proc; and `shadowingProc` has a single proc which immediately redefines a variable.

These tests rely on `Primitives.hs`, which contains a few simple arrows used throughout the tests. We also provide `SF.hs`, which provides a very simple arrow like those used in Arrowized Functional Reactive Programming (AFRP), allowing you to test these examples.