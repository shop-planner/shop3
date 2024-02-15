#!/bin/bash

set -x

# do_tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

usage () {
    echo "$0 [lisp implementation] [test-name]*"
    echo " - run lisp tests"
    echo " - quit with exit status 0 on getting eof"
    echo " - quit with exit status >0 if an unhandled error occurs"
    echo " lisp implementations include abcl, allegro, allegromodern, ccl (clozure),"
    echo "  clisp, cmucl, ecl, ecl-bytecodes, lispworks, "
    echo "  mkcl, and sbcl."
    echo " Of these, only ccl, sbcl, and allegro are known to work, at present."
    echo " The precise command to be used to invoke the lisp (if, e.g., it is not on"
    echo "    your PATH), may be set by setting shell environment variables, which"
    echo "    generally (but not always!) are simply the implementation name in all caps."
    echo " test-names could be test-shop-unifier.lisp, test-shop3.lisp, or "
    echo "   test-shop3-satellite.lisp.  If this argument is not supplied, all tests"
    echo "   will be run. If supplied, the lisp implementation MUST BE supplied."
    echo "OPTIONS:"
    echo "    -d -- debug mode"
    echo "    -b -- build only; do not run tests."
    echo "    -u -h -- show this message."
}

unset DEBUG_ASDF_TEST
unset BUILD_ONLY

# Allegro has some troubles with stack size sometimes
if [ `ulimit -s` -lt 32768 ]; then
    ulimit -s 32768
fi

while getopts "duhb" OPTION
do
    case $OPTION in
        d)
            export DEBUG_ASDF_TEST=t
            ;;
        b)
            export BUILD_ONLY=t
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

if [ $# -gt 0 ] ; then
    scripts=$*
else
    scripts="test-shop3.lisp test-shop3-satellite.lisp test-shop-unifier.lisp"
fi

sok=1

DO () { ( set -x ; "$@" ); }


do_tests() {
  command="$1" eval="$2"
  ( DO $command $eval '(load "build-shop3.lisp")' )
  if [ $? -ne 0 ] ; then
    echo "Compilation FAILED" >&2
    exit $?
  else
    echo "Compiled OK" >&2
    if [ "${BUILD_ONLY}x" != "x" ]; then
        echo success > tmp/results/status
        exit 0
    fi
    compile_val=0
    test_count=0
    test_pass=0
    test_fail=0
    failed_list=""
    for i in $scripts ;
    do
      echo "Testing: $i" >&2
      test_count=`expr "$test_count" + 1`
      if [ $lisp = "lispworks" ]; then
          if DO $command $eval "(setf system:*stack-overflow-behaviour* :warn)" $eval "(load \"$i\")" ; then
              echo "Using $command, $i passed" >&2
	      test_pass=`expr "$test_pass" + 1`
          else
              echo "Using $command, $i failed" >&2
	      test_fail=`expr "$test_fail" + 1`
	      failed_list="$failed_list $i"
              sok=0
          fi
      else
          if DO $command $eval "(load \"$i\")" ; then
              echo "Using $command, $i passed" >&2
	      test_pass=`expr "$test_pass" + 1`
          else
              echo "Using $command, $i failed" >&2
	      test_fail=`expr "$test_fail" + 1`
	      failed_list="$failed_list $i"
              sok=0
          fi
      fi
    done
    echo >&2
    echo "-#---------------------------------------" >&2
    echo "Using $command" >&2
    echo "Ran $test_count tests: " >&2
    echo "  $test_pass passing and $test_fail failing" >&2
    if [ $test_fail -eq 0 ] ; then
	echo "all tests apparently successful" >&2
        echo success > tmp/results/status
    else
	echo "failing test(s): $failed_list" >&2
    fi
    echo "-#---------------------------------------" >&2
    echo >&2
  fi
}

# terminate on error
set -e

command= flags= nodebug= eval=
case "$lisp" in
  abcl)
    command="${ABCL:-abcl}"
    flags="--noinit --nosystem --noinform"
    eval="--eval" ;;
  allegro)
    command="${ALLEGRO:-alisp}"
    # flags="-q -e (sys:resize-areas :new 10000000000 :old 10000000000 :old-code 5000000000)"
    flags="-q -ee %28sys%3aresize%2dareas%20%3anew%2010000000000%20%3aold%2010000000000%20%3aold%2dcode%205000000000%29"
    nodebug="-batch"
    eval="-e" ;;
    # allegromodern won't work... [2012/10/09:rpg]
  allegromodern)
    command="${ALLEGROMODERN:-mlisp}"
    # flags="-q -e (sys:resize-areas :new 10000000000 :old 10000000000 :old-code 5000000000)"
    flags="-q -ee %28sys%3aresize%2dareas%20%3anew%2010000000000%20%3aold%2010000000000%20%3aold%2dcode%205000000000%29"
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
  lispworks)
    command="${LISPWORKS:-lispworks}"
    flags="-siteinit - -init -"
    eval="-eval" ;;
  mkcl)
    command="${MKCL:-mkcl}"
    flags="-norc"
    eval="-eval" ;;
  sbcl)
    command="${SBCL:-sbcl}"
    flags="--dynamic-space-size 4000 --noinform --end-runtime-options --no-userinit --no-sysinit"
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

# FIXME: probably need to ensure that we are in the right directory
# before we do the following...[2019/04/23:rpg]
SHOP3DIR="$(cd ../shop3 ; /bin/pwd)"
THISDIR="$(pwd)"
PATH=${THISDIR}/VAL:$PATH
type -P validate 2>/dev/null
if [[ $? != 0 ]];
then
    echo "validate is not found in path.  Probably you haven't built it"  >&2
    exit 44
fi
#     echo "validate is not found in path.  Trying to build it."  >&2
#     pushd ${THISDIR}/ext/VAL
#     make
#     if [[ $? != 0 ]];
#     then 
#        echo "Unable to build validate." >&2
#        exit 44
#     else
#         type -P validate 2>/dev/null
#         if [[ $? != 0 ]];
#         then
#             echo "After building validate, unable to find it in PATH." >&2
#             exit 45
#         fi
#     fi
# fi


# terminate on error
set -e

export CL_SOURCE_REGISTRY="${SHOP3DIR}:${THISDIR}//"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${SHOP3DIR}\" (\"${THISDIR}/tmp/fasls\" :implementation)) :ignore-inherited-configuration)"

command="$command $flags"
if [ -z "${DEBUG_ASDF_TEST}" ] ; then
  command="$command $nodebug"
fi


create_config () {
    mkdir -p tmp/test-source-registry-conf.d tmp/test-asdf-output-translations-conf.d
}

clean_up () {
    rm -rf tmp/test-source-registry-conf.d tmp/test-asdf-output-translations-conf.d
}

if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
else
    mkdir -p tmp
    create_config
    mkdir -p tmp/results
    echo failure > tmp/results/status
    thedate=`date "+%Y-%m-%d"`
    do_tests "$command" "$eval" 2>&1 | \
	tee "tmp/results/${lisp}.text" "tmp/results/${lisp}-${thedate}.save"
    read a < tmp/results/status
    clean_up
    [ success = "$a" ] ## exit code
fi
