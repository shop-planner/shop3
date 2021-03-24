
![SHOP3 logo](https://github.com/shop-planner/shop-master/raw/master/img/shop3-superclarendon-trattatello-small.png )

# SHOP3 

This repository contains the open source version of the SHOP3 planner.

## License

[Mozilla Public License](https://www.mozilla.org/en-US/MPL/)

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
* **LispWorks**: Recently we have been given a license to test SHOP3 on
  [LispWorks](http://www.lispworks.com/), and have verified that the SHOP3 test
  suite passes on LW.  Like Allegro, LispWorks is a *commercial* lisp implementation.

We regularly test SHOP3 on all of these lisp implementations,
using an exhaustive test suite.  There are at least a handful of
other CL implementations, both open source and commercial, but we have
not tested them with SHOP3.  Attempts to run SHOP3 on ECL, ABCL, and clisp
have *not* been successful.  Patches to make SHOP3 work with other
implementations are welcomed, but we have no plans to add support to
any others ourselves.

Note that we have not tested SHOP3 on the free versions of ACL or LispWorks, and
we do not expect it to work well on these platforms because of their resource limitations.
For hobbyist projects, we suggest SBCL or CCL instead.

### Obtaining the code

Clone this repository and then **initialize and populate the
submodules**.

```
git clone https://github.com/shop-planner/shop3
```

See the two installation options below to see whether you need to supply
the `--recurse-submodules` option when cloning.

Once you have done that, you can get SHOP3 started either using
Quicklisp (probably easiest) or by configuring ASDF (the Common Lisp
build manager) yourself.  See below.

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

Quicklisp contains all of the dependencies that SHOP3 requires *except* the
`"pddl-utils"` system.  This system is only used in the tests, or for some optional
functionality.  If you wish to have this subsystem, you can either;

1. populate the git submodules -- or just the `pddl-tools` one -- in the SHOP3 git
  repository, OR
2. Clone [the pddl-tools repository](https://github.com/rpgoldman/pddl-tools) into 
  `local-projects`, as well, and do not bother with the git submodules.

**Note:** At the moment, SHOP3 is not available for installation through
Quicklisp, but we have requested that it be added to the distribution,
and hope it will be.  If it is, we will update the README accordingly, and
installation will become much simpler.

### Option 2: Without Quicklisp

The SHOP3 repository has links to all of the libraries it requires in
its git submodules.  If Quicklisp is not the right approach for you,
you must use git commands to populate SHOP3's submodules (if you like,
you may clone with the `--recurse-submodules` option), and then:

1. Start CL
2. Load the [ASDF](https://www.common-lisp.net/project/asdf/) CL build
   system by doing `(require :asdf)` in the CL REPL.
3. Tell ASDF where to find SHOP3:
   ```
   (asdf:initialize-source-registry
            '(:source-registry (:tree SHOP3-DIRECTORY) :inherit-configuration)
   ```
   Fill in the name of the directory of your cloned repository for `SHOP3-DIRECTORY`.
4. Enter `(asdf:load-system "shop3")` into the CL REPL.

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



<!--
Local Variables:
mode: markdown
End:
-->
