#!/bin/bash
export SHOP_DIR=/home/jmaraist/Shop/trunk/
export NST_DIR=/home/jmaraist/Lib/Lisp/nst/
export CLOSER_MOP_DIR=/home/jmaraist/Lib/Lisp/SIFT/closer-mop/

##  #################################################################
##  The lines below can be copied in to the jenkins script box on
##  beast.  Do NOT use the SHOP_DIR, NST_DIR and CLOSER_MOP_DIR above;
##  they will fail!
##  #################################################################

export SKIP_SHOP_ALLEGRO_MIXEDCASE=1
export SHOP_ALLEGRO_MIXEDCASE=/usr/local/acl/acl82/mlisp
#export SKIP_SHOP_ALLEGRO_UPCASE=
export SHOP_ALLEGRO_UPCASE=/usr/local/acl/acl82/alisp
export SKIP_SHOP_ALLEGRO_MIXEDCASE8=1
export SHOP_ALLEGRO_MIXEDCASE8=/usr/local/acl/acl82/mlisp8
export SKIP_SHOP_ALLEGRO_UPCASE8=1
export SHOP_ALLEGRO_UPCASE8=/usr/local/acl/acl82/alisp8

#export SKIP_SHOP_SBCL_UPCASE=
export SHOP_SBCL_UPCASE=/usr/local/bin/sbcl

export SKIP_SHOP_CCL_UPCASE=1
export SHOP_CCL_UPCASE=/home/jmaraist/Lib/Lisp/clozure/ccl/lx86cl
export SKIP_SHOP_CCL64_UPCASE=1
export SHOP_CCL64_UPCASE=/home/jmaraist/Lib/Lisp/clozure/ccl/lx86cl64

export SKIP_SHOP_CLISP_UPCASE=1
export SHOP_CLISP_UPCASE=
export SKIP_SHOP_CLISP_MIXEDCASE=1
export SHOP_CLISP_MIXEDCASE=

export SKIP_SHOP_LW_UPCASE=1
export SHOP_LW_UPCASE=

#################################################################

./jenkins.perl
