#!/bin/sh

if [ $# -eq 0 ]
then

  i=1

  while [ $i -le 20 ]
  do
    ../../../Validator/validate ../adlsimpleTimeSat.pddl pfile${i} pfile${i}.soln
    echo ---------------------------------------------
    i=`expr $i + 1`
  done

else
  i=${1}
  ../../../Validator/validate -v ../adlsimpleTimeSat.pddl pfile${i} pfile${i}.soln
fi

