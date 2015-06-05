#!/bin/sh

if [ $# -eq 0 ]
then

  i=1

  while [ $i -le 22 ]
  do
    ../../../Validator/validate ../stdepots.pddl pfile${i} pfile${i}.soln
    echo ---------------------------------------------
    i=`expr $i + 1`
  done

else
  i=${1}
  ../../../Validator/validate -v ../stdepots.pddl pfile${i} pfile${i}.soln
fi

