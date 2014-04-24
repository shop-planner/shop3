#!/bin/sh

# do_tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

usage () {
    echo "$0 [lisp]"
    echo " - Build SHOP2 using lisp implementation from command line."
    echo " - quit with exit status 0 on getting eof"
    echo " - quit with exit status >0 if an unhandled error occurs"
    echo " lisps include abcl, allegro, ccl (clozure),"
    echo "  clisp, cmucl, ecl, gclcvs, sbcl, and scl."
    echo "OPTIONS:"
    echo "    -d -- debug mode"
    echo "    -u -h -- show this message."
}

unset DEBUG_TEST

while getopts "duh" OPTION
do
    case $OPTION in
        d)
            export DEBUG_TEST=t
            ;;
        u)
            usage
            exit 1
            ;;
        h)
            usage
            exit 1
            ;;
    esac
done
shift $(($OPTIND - 1))

if [ x"$1" = "xhelp" ]; then
    usage
    exit 1
fi
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
else
    ( DO $command $eval '(load "build-shop2.lisp")' )
    if [ $? -ne 0 ]; then
        echo "Build SHOP2 failed" >&2
        exit 1
    else
        echo "BUILD SHOP2 passed" >&2
        exit 0
    fi
fi

