Welcome to SHOP2, 2.9.0

This release represents a first attempt to seriously address
portability for SHOP2.  In particular, with this release we
restructured the tests using the FiveAM library, which supports more
lisp implementations than SIFT's NST testing library.  We have tested
SHOP2 on Allegro Common Lisp, Steel Bank Common Lisp (SBCL), Clozure
Common Lisp, and GNU clisp.

We do not recommend the use of clisp with SHOP2, although we are
certainly interested in supporting it, and would be happy to accept
patches and fixes.  In particular, more difficult planning problems
can cause stack exhaustion.  On all but clisp, compilation with

(DECLAIM (OPTIMIZE (SPEED 3) (SPACE 3)))

avoids this problem.  On clisp, some tests fail with stack exhaustion
errors.  Also, on clisp, unlike the other lisps, a stack exhaustion
error is *not* a Common Lisp condition, and cannot be successfully
handled (or even aborted from).  If you must run with GNU clisp,
please use the ANSI-compliance option.  If you need a free Common Lisp
implementation, we urge you to use SBCL or Clozure Common Lisp
instead.

We have not had the time to test on other Common Lisps, such as ECL,
ABCL, or CMU CL.  We suspect SHOP2 would work poorly on ABCL, because
of the JVM's restrictions on tail call optimization.  We would welcome
any test results or reports of experience with as-yet-unsupported CL
implementations.

Testing SHOP2 requires three ancillary libraries:
+ FiveAM test library
+ Arnesi utility library, required for building FiveAM
+ FiveAM ASDF tester library, which makes ASDF:TEST-SYSTEM on SHOP2
invoke the FiveAM tests correctly.

We make these libraries available in separate tarballs for the
interested user.  Please report any test problems.

Previous entries in the README/Changelog are retained below.

---

This is a release candidate for SHOP2 (lisp version), version 2.  This
version inaugurates a gradual shift in the architecture of SHOP2, and
introduces support for PDDL domain descriptions.  Both of these are in
early stages.  We have also separated out SHOP's theorem-prover and
SHOP's unification algorithm as subsystems.  There are also a small
number of bugfixes.

Previous versions of SHOP2 were essentially written in pre-CLOS (the
Common Lisp Object System) Common Lisp.  This has caused problems for
developers wishing to add new capabilities (such as temporal planning,
planning in nondeterministic domains, etc.) to the core SHOP
algorithms.  Those new capabilities had to be added in the form of
redefinitions of core SHOP2 functions, generally by simply copying the
entire shop2 body of code and modifying it.  This practice created (at
least) two problems:

1.  If bugs were fixed on the main branch of SHOP2, the fixes were
difficult to move into alternate versions;

2.  It was very difficult to merge together these augmented versions
of SHOP.  For example, if one had a temporal version of SHOP, and a
nondeterministic version of SHOP, there was no obvious path forward to
a nondeterministic, temporal version of SHOP.

We are gradually moving to make SHOP a thoroughly object-oriented
system, so that behavior tailoring can be done by simply providing new
methods for existing generic functions.  We have been cautious in
doing so, for now, so that the main path to specializing SHOP behavior
is to define a subclass of the DOMAIN class, and provide new generic
method definitions dispatching on this class.  Elnatan Reisner has
also rewritten the existing SHOP state code --- which provided
alternative state representations --- to use CLOS method dispatch;
previously the equivalent of method dispatch was hand-coded.  This
will allow extension of the state representations, as well (but note
that states are implemented as structures, rather than CLOS objects,
for greater efficiency).  We expect to provide CLOS-based
encapsulation of planner settings and planner state as we move
forward.

We have added rudimentary support for PDDL domain definitions in the
PDDL-DOMAIN subclass of DOMAIN.  This support is documented in the
manual supplied with this release.

We have also created two sub-systems of SHOP, the SHOP theorem-prover
and the SHOP unifier, each in its own package.  Doing so provides two
advantages:

1.  SHOP is a very large program, and separating out these subsystems
provides a more understandable API for these major components, as well
as helping to avoid problems of namespace clutter.

2.  These two core components may be separately useful.  We have
successfully used SHOP's theorem-prover in different state-based
applications.

We hope that you enjoy using SHOP2.  Please report any bugs you find
using the Sourceforge bugtracking database.

Robert P. Goldman, SIFT, LLC
Dana Nau, University of Maryland
Ugur Kuter, University of Maryland

Release notes from the previous release:

This is primarily a bugfix release to follow on to SHOP2 1.2.  We
expect that this will be the last release in SHOP2 1.x, aside from
bugfixes, and that further effort will move to a new version 2.x that
will have a substantially new architecture to better support modular,
object-oriented extensions to SHOP2 and integrating SHOP2 into larger
applications.

There are two substantial changes from SHOP2 1.2 to 1.3:

1.  The new default loading method is to use the ASDF (Another System
Definition Facility) system loader.  Those unfamiliar with ASDF can
see the web page http://www.cliki.net/asdf for more details.  However,
it is very likely that if you have a modern Common Lisp
implementation, ASDF is already distributed with your common lisp.  A
good first test in loading SHOP2 is to do the following:

a.  make a symbolic link from your asdf system definition file (this
will be system dependent) to shop2.asd.  Do NOT copy the file; link it.

If you don't know where this is, you can try starting up your lisp
implementation and doing the following:

(require :asdf) [if this fails, you need to install asdf, see the above website]
asdf:*central-registry* [this should print a list of directories to hold asd links.]

b.  start your lisp compiler

c.  (require :asdf) --- if this doesn't work, obtain and install a
copy of asdf, using the above web site.

d.  (asdf:oos 'asdf:load-op :shop2)

If you are lucky (users of up-to-date ACL and SBCL may expect to be
lucky), this will Just Work.

2.  There is now an extensive regression test suite for SHOP2.  To
run the regression test suite, you may type:

(asdf:oos 'asdf:test-op :shop2)

WARNING:  this may take a couple of days to finish!  This runs all the
domain descriptions distributed with SHOP2, and checks the results
against saved plans.

We would be particularly interested in getting bug reports (or, better
yet, patches!) from people who have tried to use SHOP2 with lisps
other than Allegro and SBCL and on platforms other than Linux.  We
would also be interested in hearing from people who have run the
regression test suite.


This distribution contains the following files:

shop2.lisp  The SHOP2 program; at the top of the program file
            is the SHOP2 license

state-utils.lisp
            Additional source code for SHOP2.  A first step in
	    decomposing SHOP2 into mutliple files.

shop2-<foo>.pdf   The SHOP2 documentation (in Adobe Acrobat format)

shop2-<foo>.doc	  The SHOP2 documentation (in MS Word format)

install.lisp    This script compiles shop2.lisp into a
                form that both loads and runs faster.  It uses a
                function called "compile-file", which is available
                in Allegro Lisp 6.0.

		This file is largely obsolete now.  You should
                probably be using ASDF to laod the system instead.

shop2.asd	ASDF system definition for SHOP2 system.  This is now
                the preferred means of loading shop2.  Other methods
                will soon be deleted unless someone else is interested
                in maintaining them.

shop2.system	MK-DEFSYSTEM system definition for SHOP2.  This
                definition is less obsolete than INSTALL.LISP, but
                is also falling into bit rot and will no longer be
                maintained unless someone volunteers.

ji4shop2/       The Java interface for SHOP2.

examples/   Example domains in seperate subdirectores:

 depots/    The Depots domain from the third international planning
            competition (at AIPS-2002)

 UMT2/	    The UM Translog 2 domain from the third international planning
            competition.

 toy/       Some very simple toy examples

 logistics/  A simple logistics planning domain

 blocks/     A relatively sophisticated encoding of the traditional
             blocks-world planning domain

    See documentation files (e.g., README.txt) in the example
    directories for more information about those examples.

IPC-2000/       The logistics domain in the second international planning
                competition (at AIPS-2000).  This directory contains the
                solution validator provided in the third International
                Planning Competition at AIPS-2002.

		[This directory should probably have been placed in the
                examples.]
