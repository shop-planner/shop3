#!/bin/bash
export SHOP_DIR=/home/jm/Shop/trunk/
export NST_DIR=/home/jm/Lib/Lisp/nst/
export CLOSER_MOP_DIR=/home/jm/Lib/Lisp/SIFT/closer-mop/

export SKIP_SHOP_ALLEGRO_MIXEDCASE=1
export SHOP_ALLEGRO_MIXEDCASE=`which mlisp`

# export SKIP_SHOP_ALLEGRO_UPCASE=1
export SHOP_ALLEGRO_UPCASE=`which alisp`

export SKIP_SHOP_ALLEGRO_MIXEDCASE8=1
export SHOP_ALLEGRO_MIXEDCASE8=`which mlisp8`

export SKIP_SHOP_ALLEGRO_UPCASE8=1
export SHOP_ALLEGRO_UPCASE8=`which alisp8`

# export SKIP_SHOP_SBCL_UPCASE=1
export SHOP_SBCL_UPCASE=`which sbcl`

export SKIP_SHOP_CLISP_UPCASE=1
export SHOP_CLISP_UPCASE=`which clisp`

export SKIP_SHOP_CLISP_MIXEDCASE=1
export SHOP_CLISP_MIXEDCASE=`which clisp`

export SKIP_SHOP_CCL_UPCASE=1
export SHOP_CCL_UPCASE=~/Lib/Lisp/clozure/ccl/lx86cl

export SKIP_SHOP_LW_UPCASE=1
export SHOP_LW_UPCASE=/usr/local/lib/LispWorksPersonal/lispworks-personal-6-0-1-x86-linux

./jenkins.perl