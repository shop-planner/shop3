
![SHOP3 logo](https://github.com/shop-planner/shop-master/raw/master/img/shop3-superclarendon-trattatello-small.png )

# SHOP3 

This repository contains the open source version of the SHOP3 planner.

<!-- Remember to add a pointer to the paper. -->

## Installation Instructions

### Common Lisp
SHOP3 is written in *Common Lisp* (CL),  and requires a CL
implementation to run.  We recommend one of the following
implementations:

* **SBCL**: Steel Bank Common Lisp.  Available in packages on Ubuntu
  Linux and Mac Homebrew.  Also available from [sbcl.org](https://sbcl.org).

* **CCL**: Clozure Common Lisp.  Available from
  [clozure.com](https://ccl.clozure.com).

* **ACL**: Allegro Common Lisp: Available from [franz.com](https://franz.com).
  Unlike the previous two implementations, this is a *commercial*
  software product.

We regularly test SHOP3 on all three of these lisp implementations,
using an exhaustive test suite.  There are at least a handful of
other CL implementations, both open source and commercial, but we have
not tested them with SHOP3.  Patches to make SHOP3 work with other
implementations are welcomed, but we have no plans to add support to
any others ourselves.

### Option 1: Quicklisp

Probably the easiest way to work with SHOP3 will be to use it in
conjunction with [Quicklisp](https://beta.quicklisp.org).  Quicklisp
is a package manager for CL libraries, and will be the easiest way to
get the libraries that SHOP3 depends on.

If you wish to use Quicklisp, follow the installation instructions for
quicklisp at the above site, and then clone the SHOP3 repository into
the `local-projects` subdirectory of your Quicklisp install
directory.  Once this is done, you should be able to:

1. Start CL
2. Inside CL, enable Quicklisp
3. Enter `(ql:quickload "shop3")` into the CL REPL.

### Option 2: Without Quicklisp

The SHOP3 repository has links to all of the libraries it requires in
its git submodules.  If Quicklisp is not the right approach for you,
you may use git commands to populate SHOP3's submodules, and then:

1. Start CL

2. Load the [ASDF](https://www.common-lisp.net/project/asdf/) CL build
   system by doing `(require :asdf)` in the CL REPL.

3. Tell ASDF where to find SHOP3:

        (asdf:initialize-source-registry
            '(:source-registry (:tree SHOP3-DIRECTORY) :inherit-configuration)

  
  Fill in the name of the directory of your cloned repository for `SHOP3-DIRECTORY`.

4. `(asdf:load-system "shop3")`

### Once started

We suggest that you interact with SHOP3 through the `shop-user`
package in the REPL.

The best environment for working with CL for any purpose, including
SHOP3 planning, is through the Superior Lisp Interaction Mode for
Emacs, or [SLIME](https://common-lisp.net/project/slime/).  If you use
ACL, Franz have their own Emacs Lisp Interface (ELI) as a possible
alternative.  Both ACL and CCL have some sort of GUI, as well.

## Using SHOP3

See the user manual in the `docs` directory of the repository.
Building the manual requires having texinfo, and SBCL installed, and
benefits from having tex/latex installed.  To build, change to the
`docs` directory and do `make html` if you have only makeinfo
installed, or just `make` if you have tex/latex, as well.  This will
produce the manual in HTML, Emacs info, and PDF formats.



<!--
Local Variables:
mode: markdown
End:
-->
![SHOP3 logo](https://github.com/shop-planner/shop-master/raw/master/img/shop3-superclarendon-trattatello-small.png )

# SHOP3 

This repository contains the open source version of the SHOP3 planner.

<!-- Remember to add a pointer to the paper. -->

## Installation Instructions

### Common Lisp
SHOP3 is written in *Common Lisp* (CL),  and requires a CL
implementation to run.  We recommend one of the following
implementations:

* **SBCL**: Steel Bank Common Lisp.  Available in packages on Ubuntu
  Linux and Mac Homebrew.  Also available from [sbcl.org](https://sbcl.org).

* **CCL**: Clozure Common Lisp.  Available from
  [clozure.com](https://ccl.clozure.com).

* **ACL**: Allegro Common Lisp: Available from [franz.com](https://franz.com).
  Unlike the previous two implementations, this is a *commercial*
  software product.

We regularly test SHOP3 on all three of these lisp implementations,
using an exhaustive test suite.  There are at least a handful of
other CL implementations, both open source and commercial, but we have
not tested them with SHOP3.  Patches to make SHOP3 work with other
implementations are welcomed, but we have no plans to add support to
any others ourselves.

### Option 1: Quicklisp

Probably the easiest way to work with SHOP3 will be to use it in
conjunction with [Quicklisp](https://beta.quicklisp.org).  Quicklisp
is a package manager for CL libraries, and will be the easiest way to
get the libraries that SHOP3 depends on.

If you wish to use Quicklisp, follow the installation instructions for
quicklisp at the above site, and then clone the SHOP3 repository into
the `local-projects` subdirectory of your Quicklisp install
directory.  Once this is done, you should be able to:

1. Start CL
2. Inside CL, enable Quicklisp
3. Enter `(ql:quickload "shop3")` into the CL REPL.

### Option 2: Without Quicklisp

The SHOP3 repository has links to all of the libraries it requires in
its git submodules.  If Quicklisp is not the right approach for you,
you may use git commands to populate SHOP3's submodules, and then:

1. Start CL

2. Load the [ASDF](https://www.common-lisp.net/project/asdf/) CL build
   system by doing `(require :asdf)` in the CL REPL.

3. Tell ASDF where to find SHOP3:

  ```
   (asdf:initialize-source-registry
     '(:source-registry (:tree SHOP3-DIRECTORY) :inherit-configuration)
    ```
  
    Fill in the name of the directory of your cloned repository for `SHOP3-DIRECTORY`.

4. `(asdf:load-system "shop3")`

### Once started

We suggest that you interact with SHOP3 through the `shop-user`
package in the REPL.

The best environment for working with CL for any purpose, including
SHOP3 planning, is through the Superior Lisp Interaction Mode for
Emacs, or [SLIME](https://common-lisp.net/project/slime/).  If you use
ACL, Franz have their own Emacs Lisp Interface (ELI) as a possible
alternative.  Both ACL and CCL have some sort of GUI, as well.

## Using SHOP3

See the user manual in the `docs` directory of the repository.
Building the manual requires having texinfo, and SBCL installed, and
benefits from having tex/latex installed.  To build, change to the
`docs` directory and do `make html` if you have only makeinfo
installed, or just `make` if you have tex/latex, as well.  This will
produce the manual in HTML, Emacs info, and PDF formats.



<!--
Local Variables:
mode: markdown
End:
-->
