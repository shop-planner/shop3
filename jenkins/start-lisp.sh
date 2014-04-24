#!/bin/sh

# This is a somewhat misnamed script: What this does is consume the
# lisp environment variable (defaulting to "sbcl"), and set the
# "lisp_command" and "lisp_eval" variables, and export them, making it
# possible for callers to invoke an arbitrary lisp, and the DO()
# function.

lisp=${1:-sbcl} ; shift

# terminate on error
set -e

DO () { ( set -x ; "$@" ); }


command= flags= nodebug= eval=
case "$lisp" in
  abcl)
    command="${ABCL:-abcl}"
    flags="--noinit --nosystem --noinform"
    eval="--eval" ;;
  allegro)
    command="${ALLEGRO:-alisp}"
    flags="-q"
    nodebug="-batch"
    eval="-e" ;;
    # allegromodern won't work... [2012/10/09:rpg]
  allegromodern)
    command="${ALLEGROMODERN:-mlisp}"
    flags="-q"
    nodebug="-batch"
    eval="-e" ;;
  ccl)
    command="${CCL:-ccl}"
    flags="--no-init --quiet"
    nodebug="--batch"
    eval="--eval" ;;
  clisp)
    command="${CLISP:-clisp}"
    flags="-norc -ansi -I -m 12MB"
    nodebug="-on-error exit"
    eval="-x" ;;
  cmucl)
    # cmucl likes to have its executable called lisp, but so does scl
    # Please use a symlink or an exec ... "$@" trampoline script.
    command="${CMUCL:-cmucl}"
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  ecl)
    command="${ECL:-ecl}"
    flags="-norc -load sys:cmp"
    eval="-eval" ;;
  ecl-bytecodes)
    command="${ECL:-ecl}"
    flags="-norc -eval (ext::install-bytecodes-compiler)"
    eval="-eval" ;;
  # gclcvs)
  #   export GCL_ANSI=t
  #   command="${GCL:-gclcvs}"
  #   flags="-batch"
  #   eval="-eval" ;;
  # lispworks)
  #   command="${LISPWORKS:-lispworks}"
  #   # If you have a licensed copy of lispworks,
  #   # you can obtain the "lispworks" binary with, e.g.
  #   # echo '(hcl:save-image "/lispworks" :environment nil)' > /tmp/build.lisp ;
  #   # ./lispworks-6-0-0-x86-linux -siteinit - -init - -build /tmp/build.lisp
  #   flags="-siteinit - -init -"
  #   eval="-eval" ;;
  # mkcl)
  #   command="${MKCL:-mkcl}"
  #   flags="-norc"
  #   eval="-eval" ;;
  sbcl)
    command="${SBCL:-sbcl}"
    flags="--noinform --userinit /dev/null --sysinit /dev/null"
    nodebug="--disable-debugger"
    eval="--eval" ;;
  # scl)
  #   command="${SCL:-scl}"
  #   flags="-noinit"
  #   nodebug="-batch"
  #   eval="-eval" ;;
  # xcl)
  #   command="${XCL:-xcl}"
  #   flags="--no-userinit --no-siteinit"
  #   eval="--eval" ;;
  *)
    echo "Unsupported lisp: $1" >&2
    echo "Please add support to run-tests.sh" >&2
    exit 42 ;;
esac

if ! type "$command" ; then
    echo "lisp implementation not found: $command" >&2
    exit 43
fi

command="$command $flags"
if [ -z "${DEBUG_TEST}" ] ; then
  command="$command $nodebug"
fi


if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
fi

export lisp_command=${command}
export lisp_eval=${eval}
