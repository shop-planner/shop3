#!/bin/sh

if [ $# -eq 0 ]
then

  i=1

  while [ $i -le 22 ]
  do
    ../../../Validator/validate ../tdepots.pddl pfile${i} pfile${i}.soln
    echo ---------------------------------------------
    i=`expr $i + 1`
  done

else
  i=${1}
  ../../../Validator/validate -v ../tdepots.pddl pfile${i} pfile${i}.soln
fi

