This directory contains a number of extremely simple toy examples
which illustrate various features of SHOP2.  Each of the Lisp files in
this directory contains exactly one SHOP2 domain description plus one
or more problem descriptions and one or more invocations of the
planning process.  To execute one of these examples, start Lisp and
load the file; SHOP2 will begin running automatically.

The best example for new users to start with is basic-example.lisp.
This file demonstrates some of the simplest features of SHOP2.  The
file basic-example.txt shows the text of a sample Lisp session
using that example.

The remaining files each illustrate a specific interesting feature of
SHOP2; see the corresponding portions of the SHOP2 documentation for
more details.  The file ordering-example.lisp illustrates the
combination of ordered and unordered task lists.  The file
optimize-example.lisp illustrates features for branch-and-bound
optimization of plan costs.  The file sort-example.lisp illustrates
sorting of alternative method bindings.  The file unbound-example.lisp
illustrates the use of unbound variables in operator invocations which
allow plans to include values computed within the operator.
