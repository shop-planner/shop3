# Building SHOP applications with `buildapp`

`buildapp` is Zach Beane's tool for building stand-alone applications in Common Lisp, compatible with both SBCL and CCL.  For more details, see [its webpage](https://www.xach.com/lisp/buildapp/).

If you have buildapp installed, you can use it to build SHOP-based applications with the contents of this directory.

The applications that can be built are:

- `ess-shop` -- given a domain and a problem, output a plan for the problem using `find-plans-stack`.
- `shop` -- given a domain and a problem, output a plan for the problem using `find-plans` ("classic SHOP").  Note that this does not offer the full functionality of `ess-shop`.
- `tree-compare` -- compare two files that contain SHOP planning trees.

# Build instructions

It is sufficient to do `make install` to get the above three applications installed, if `buildapp` is in your `PATH`.  Note that the Makefile supports the standard `DESTDIR` make variable.  For example, I use `make install DESTDIR=~/.local/`

# For assistance

For questions, post discussions in the GitHub SHOP3 project. Report bugs in issues.
