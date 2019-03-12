#!/bin/sh

if [ $# -eq 0 ]
then

  i=1

  while [ $i -le 20 ]
  do
    ../../../Validator/validate -t 0.001 ../zenoSimpleTime.pddl pfile${i} pfile${i}.soln
    echo ---------------------------------------------
    i=`expr $i + 1`
  done

else
  i=${1}
  ../../../Validator/validate -t 0.001 -v ../zenoSimpleTime.pddl pfile${i} pfile${i}.soln
fi

