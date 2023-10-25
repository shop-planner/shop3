#!/bin/bash

if type ghead &> /dev/null ; then
   HEAD=ghead ;
else
    HEAD=head
fi

if type grealpath &> /dev/null ; then
   REALPATH=grealpath ;
else
    REALPATH=realpath
fi

THISDIR=$(${REALPATH} .)

echo "THISDIR = $THISDIR"

shop_plan_only () {
    perl -ne 'BEGIN {$/="======================================================================
"; $i=0;} if ( $i == 0 ) { $i++; } elsif ($i == 1) { print $_; exit 0; }' | ${HEAD} -n -1
}

EXPECTED=$(
cat <<'END_HEREDOC'
  1: (!DRIVE-TRUCK TRUCK4-1 LOC4-1 LOC4-3): 1.00
  2: (!LOAD-TRUCK PACKAGE3 TRUCK4-1 LOC4-3): 1.00
  3: (!DRIVE-TRUCK TRUCK3-1 LOC3-1 LOC3-3): 1.00
  4: (!LOAD-TRUCK PACKAGE4 TRUCK3-1 LOC3-3): 1.00
  5: (!DRIVE-TRUCK TRUCK8-1 LOC8-1 LOC8-3): 1.00
  6: (!LOAD-TRUCK PACKAGE5 TRUCK8-1 LOC8-3): 1.00
  7: (!DRIVE-TRUCK TRUCK7-1 LOC7-1 LOC7-2): 1.00
  8: (!LOAD-TRUCK PACKAGE7 TRUCK7-1 LOC7-2): 1.00
  9: (!DRIVE-TRUCK TRUCK6-1 LOC6-1 LOC6-3): 1.00
 10: (!LOAD-TRUCK PACKAGE8 TRUCK6-1 LOC6-3): 1.00
 11: (!DRIVE-TRUCK TRUCK3-1 LOC3-3 LOC3-2): 1.00
 12: (!LOAD-TRUCK PACKAGE11 TRUCK3-1 LOC3-2): 1.00
 13: (!DRIVE-TRUCK TRUCK2-1 LOC2-1 LOC2-2): 1.00
 14: (!LOAD-TRUCK PACKAGE13 TRUCK2-1 LOC2-2): 1.00
 15: (!DRIVE-TRUCK TRUCK1-1 LOC1-1 LOC1-3): 1.00
 16: (!LOAD-TRUCK PACKAGE14 TRUCK1-1 LOC1-3): 1.00
 17: (!ADD-PROTECTION (TRUCK-AT TRUCK3-1 LOC3-2)): 0.00
 18: (!LOAD-TRUCK PACKAGE15 TRUCK3-1 LOC3-2): 1.00
 19: (!FLY-AIRPLANE PLANE1 LOC6-1 LOC4-1): 1.00
 20: (!ADD-PROTECTION (AIRPLANE-AT PLANE2 LOC1-1)): 0.00
 21: (!DRIVE-TRUCK TRUCK4-1 LOC4-3 LOC4-1): 1.00
 22: (!UNLOAD-TRUCK PACKAGE3 TRUCK4-1 LOC4-1): 1.00
 23: (!DRIVE-TRUCK TRUCK3-1 LOC3-2 LOC3-1): 1.00
 24: (!UNLOAD-TRUCK PACKAGE4 TRUCK3-1 LOC3-1): 1.00
 25: (!DRIVE-TRUCK TRUCK8-1 LOC8-3 LOC8-1): 1.00
 26: (!UNLOAD-TRUCK PACKAGE5 TRUCK8-1 LOC8-1): 1.00
 27: (!FLY-AIRPLANE PLANE3 LOC1-1 LOC8-1): 1.00
 28: (!DRIVE-TRUCK TRUCK7-1 LOC7-2 LOC7-3): 1.00
 29: (!UNLOAD-TRUCK PACKAGE7 TRUCK7-1 LOC7-3): 1.00
 30: (!DRIVE-TRUCK TRUCK6-1 LOC6-3 LOC6-1): 1.00
 31: (!UNLOAD-TRUCK PACKAGE8 TRUCK6-1 LOC6-1): 1.00
 32: (!ADD-PROTECTION (TRUCK-AT TRUCK3-1 LOC3-1)): 0.00
 33: (!UNLOAD-TRUCK PACKAGE11 TRUCK3-1 LOC3-1): 1.00
 34: (!DRIVE-TRUCK TRUCK2-1 LOC2-2 LOC2-1): 1.00
 35: (!UNLOAD-TRUCK PACKAGE13 TRUCK2-1 LOC2-1): 1.00
 36: (!DRIVE-TRUCK TRUCK1-1 LOC1-3 LOC1-1): 1.00
 37: (!UNLOAD-TRUCK PACKAGE14 TRUCK1-1 LOC1-1): 1.00
 38: (!ADD-PROTECTION (TRUCK-AT TRUCK3-1 LOC3-1)): 0.00
 39: (!UNLOAD-TRUCK PACKAGE15 TRUCK3-1 LOC3-1): 1.00
 40: (!LOAD-AIRPLANE PACKAGE1 PLANE1 LOC4-1): 1.00
 41: (!FLY-AIRPLANE PLANE1 LOC4-1 LOC6-1): 1.00
 42: (!ADD-PROTECTION (AIRPLANE-AT PLANE1 LOC6-1)): 0.00
 43: (!LOAD-AIRPLANE PACKAGE2 PLANE2 LOC1-1): 1.00
 44: (!FLY-AIRPLANE PLANE2 LOC1-1 LOC5-1): 1.00
 45: (!ADD-PROTECTION (AIRPLANE-AT PLANE3 LOC8-1)): 0.00
 46: (!LOAD-AIRPLANE PACKAGE6 PLANE3 LOC8-1): 1.00
 47: (!ADD-PROTECTION (AIRPLANE-AT PLANE1 LOC6-1)): 0.00
 48: (!ADD-PROTECTION (AIRPLANE-AT PLANE1 LOC6-1)): 0.00
 49: (!LOAD-AIRPLANE PACKAGE9 PLANE1 LOC6-1): 1.00
 50: (!LOAD-AIRPLANE PACKAGE10 PLANE1 LOC6-1): 1.00
 51: (!ADD-PROTECTION (AIRPLANE-AT PLANE2 LOC5-1)): 0.00
 52: (!LOAD-AIRPLANE PACKAGE12 PLANE2 LOC5-1): 1.00
 53: (!LOAD-AIRPLANE PACKAGE5 PLANE3 LOC8-1): 1.00
 54: (!FLY-AIRPLANE PLANE3 LOC8-1 LOC4-1): 1.00
 55: (!LOAD-AIRPLANE PACKAGE8 PLANE1 LOC6-1): 1.00
 56: (!UNLOAD-AIRPLANE PACKAGE1 PLANE1 LOC6-1): 1.00
 57: (!FLY-AIRPLANE PLANE1 LOC6-1 LOC3-1): 1.00
 58: (!ADD-PROTECTION (AIRPLANE-AT PLANE1 LOC3-1)): 0.00
 59: (!ADD-PROTECTION (AIRPLANE-AT PLANE1 LOC3-1)): 0.00
 60: (!UNLOAD-AIRPLANE PACKAGE2 PLANE2 LOC5-1): 1.00
 61: (!FLY-AIRPLANE PLANE2 LOC5-1 LOC2-1): 1.00
 62: (!LOAD-AIRPLANE PACKAGE3 PLANE3 LOC4-1): 1.00
 63: (!FLY-AIRPLANE PLANE3 LOC4-1 LOC1-1): 1.00
 64: (!ADD-PROTECTION (AIRPLANE-AT PLANE3 LOC1-1)): 0.00
 65: (!ADD-PROTECTION (AIRPLANE-AT PLANE3 LOC1-1)): 0.00
 66: (!ADD-PROTECTION (TRUCK-AT TRUCK6-1 LOC6-1)): 0.00
 67: (!LOAD-TRUCK PACKAGE1 TRUCK6-1 LOC6-1): 1.00
 68: (!LOAD-AIRPLANE PACKAGE4 PLANE1 LOC3-1): 1.00
 69: (!LOAD-AIRPLANE PACKAGE11 PLANE1 LOC3-1): 1.00
 70: (!LOAD-AIRPLANE PACKAGE15 PLANE1 LOC3-1): 1.00
 71: (!FLY-AIRPLANE PLANE1 LOC3-1 LOC7-1): 1.00
 72: (!LOAD-AIRPLANE PACKAGE13 PLANE2 LOC2-1): 1.00
 73: (!FLY-AIRPLANE PLANE2 LOC2-1 LOC1-1): 1.00
 74: (!LOAD-AIRPLANE PACKAGE14 PLANE3 LOC1-1): 1.00
 75: (!UNLOAD-AIRPLANE PACKAGE5 PLANE3 LOC1-1): 1.00
 76: (!UNLOAD-AIRPLANE PACKAGE6 PLANE3 LOC1-1): 1.00
 77: (!FLY-AIRPLANE PLANE3 LOC1-1 LOC7-1): 1.00
 78: (!DRIVE-TRUCK TRUCK6-1 LOC6-1 LOC6-2): 1.00
 79: (!UNLOAD-TRUCK PACKAGE1 TRUCK6-1 LOC6-2): 1.00
 80: (!UNLOAD-AIRPLANE PACKAGE9 PLANE1 LOC7-1): 1.00
 81: (!FLY-AIRPLANE PLANE1 LOC7-1 LOC8-1): 1.00
 82: (!UNLOAD-AIRPLANE PACKAGE12 PLANE2 LOC1-1): 1.00
 83: (!FLY-AIRPLANE PLANE2 LOC1-1 LOC6-1): 1.00
 84: (!ADD-PROTECTION (TRUCK-AT TRUCK1-1 LOC1-1)): 0.00
 85: (!LOAD-TRUCK PACKAGE5 TRUCK1-1 LOC1-1): 1.00
 86: (!ADD-PROTECTION (TRUCK-AT TRUCK1-1 LOC1-1)): 0.00
 87: (!LOAD-TRUCK PACKAGE6 TRUCK1-1 LOC1-1): 1.00
 88: (!UNLOAD-AIRPLANE PACKAGE3 PLANE3 LOC7-1): 1.00
 89: (!FLY-AIRPLANE PLANE3 LOC7-1 LOC6-1): 1.00
 90: (!DRIVE-TRUCK TRUCK7-1 LOC7-3 LOC7-1): 1.00
 91: (!LOAD-TRUCK PACKAGE9 TRUCK7-1 LOC7-1): 1.00
 92: (!UNLOAD-AIRPLANE PACKAGE10 PLANE1 LOC8-1): 1.00
 93: (!FLY-AIRPLANE PLANE1 LOC8-1 LOC5-1): 1.00
 94: (!ADD-PROTECTION (TRUCK-AT TRUCK1-1 LOC1-1)): 0.00
 95: (!LOAD-TRUCK PACKAGE12 TRUCK1-1 LOC1-1): 1.00
 96: (!UNLOAD-AIRPLANE PACKAGE13 PLANE2 LOC6-1): 1.00
 97: (!DRIVE-TRUCK TRUCK1-1 LOC1-1 LOC1-2): 1.00
 98: (!UNLOAD-TRUCK PACKAGE5 TRUCK1-1 LOC1-2): 1.00
 99: (!ADD-PROTECTION (TRUCK-AT TRUCK1-1 LOC1-2)): 0.00
100: (!UNLOAD-TRUCK PACKAGE6 TRUCK1-1 LOC1-2): 1.00
101: (!ADD-PROTECTION (TRUCK-AT TRUCK7-1 LOC7-1)): 0.00
102: (!LOAD-TRUCK PACKAGE3 TRUCK7-1 LOC7-1): 1.00
103: (!UNLOAD-AIRPLANE PACKAGE14 PLANE3 LOC6-1): 1.00
104: (!DRIVE-TRUCK TRUCK7-1 LOC7-1 LOC7-2): 1.00
105: (!UNLOAD-TRUCK PACKAGE3 TRUCK7-1 LOC7-2): 1.00
106: (!ADD-PROTECTION (TRUCK-AT TRUCK8-1 LOC8-1)): 0.00
107: (!LOAD-TRUCK PACKAGE10 TRUCK8-1 LOC8-1): 1.00
108: (!UNLOAD-AIRPLANE PACKAGE8 PLANE1 LOC5-1): 1.00
109: (!FLY-AIRPLANE PLANE1 LOC5-1 LOC2-1): 1.00
110: (!ADD-PROTECTION (AIRPLANE-AT PLANE1 LOC2-1)): 0.00
111: (!ADD-PROTECTION (TRUCK-AT TRUCK1-1 LOC1-2)): 0.00
112: (!UNLOAD-TRUCK PACKAGE12 TRUCK1-1 LOC1-2): 1.00
113: (!DRIVE-TRUCK TRUCK6-1 LOC6-2 LOC6-1): 1.00
114: (!LOAD-TRUCK PACKAGE13 TRUCK6-1 LOC6-1): 1.00
115: (!ADD-PROTECTION (TRUCK-AT TRUCK7-1 LOC7-2)): 0.00
116: (!UNLOAD-TRUCK PACKAGE9 TRUCK7-1 LOC7-2): 1.00
117: (!ADD-PROTECTION (TRUCK-AT TRUCK6-1 LOC6-1)): 0.00
118: (!LOAD-TRUCK PACKAGE14 TRUCK6-1 LOC6-1): 1.00
119: (!DRIVE-TRUCK TRUCK8-1 LOC8-1 LOC8-2): 1.00
120: (!UNLOAD-TRUCK PACKAGE10 TRUCK8-1 LOC8-2): 1.00
121: (!UNLOAD-AIRPLANE PACKAGE4 PLANE1 LOC2-1): 1.00
122: (!UNLOAD-AIRPLANE PACKAGE11 PLANE1 LOC2-1): 1.00
123: (!FLY-AIRPLANE PLANE1 LOC2-1 LOC6-1): 1.00
124: (!DRIVE-TRUCK TRUCK6-1 LOC6-1 LOC6-3): 1.00
125: (!UNLOAD-TRUCK PACKAGE13 TRUCK6-1 LOC6-3): 1.00
126: (!ADD-PROTECTION (TRUCK-AT TRUCK6-1 LOC6-3)): 0.00
127: (!UNLOAD-TRUCK PACKAGE14 TRUCK6-1 LOC6-3): 1.00
128: (!ADD-PROTECTION (TRUCK-AT TRUCK2-1 LOC2-1)): 0.00
129: (!LOAD-TRUCK PACKAGE4 TRUCK2-1 LOC2-1): 1.00
130: (!ADD-PROTECTION (TRUCK-AT TRUCK2-1 LOC2-1)): 0.00
131: (!LOAD-TRUCK PACKAGE11 TRUCK2-1 LOC2-1): 1.00
132: (!UNLOAD-AIRPLANE PACKAGE15 PLANE1 LOC6-1): 1.00
133: (!DRIVE-TRUCK TRUCK2-1 LOC2-1 LOC2-2): 1.00
134: (!UNLOAD-TRUCK PACKAGE4 TRUCK2-1 LOC2-2): 1.00
135: (!ADD-PROTECTION (TRUCK-AT TRUCK2-1 LOC2-2)): 0.00
136: (!UNLOAD-TRUCK PACKAGE11 TRUCK2-1 LOC2-2): 1.00
END_HEREDOC
)

EXPECTED_TREE_FILE="${THISDIR}/test-data/tree.lisp"

compare_trees () {
    tree-compare ${EXPECTED_TREE_FILE} ${PLAN_TREE}
    local RES=$?
    if [ "$RES" -eq 2 ]
    then
        echo "Got error in tree comparison."
        exit 2
    elif [ "$RES" -eq 1 ]
    then
        echo "Plan tree result (in ${PLAN_TREE}) did not equal the expected."
        exit 1
    elif [ "$RES" -ne 0 ]
    then
        echo "Unexpected tree-compare exit status: ${RES}"
        exit 2
    fi
}

pushd ${THISDIR}/../examples/logistic

echo "Test SHOP with 2 arguments"

RES=$(shop logistic.lisp Log_ran_problems_15.lisp | shop_plan_only)
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
elif [ "$RES" != "$EXPECTED" ]; then
    echo "Plan result did not equal the expected."
    exit 1
fi

echo "Test SHOP with 1 argument"
INPUT=$(mktemp -t "inputXXXXXX")
cat logistic.lisp > ${INPUT}
cat Log_ran_problems_15.lisp >> ${INPUT}
RES=$(shop ${INPUT} | shop_plan_only)
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
elif [ "$RES" != "$EXPECTED" ]; then
    echo "Plan result did not equal the expected."
    exit 1
fi

echo "Test SHOP with 2 arguments and plan-tree"

PLAN=$(mktemp -t "planXXXXXX")
PLAN_TREE=$(mktemp -t "treeXXXXXX")
shop --tree-file ${PLAN_TREE} --plan-file ${PLAN} logistic.lisp Log_ran_problems_15.lisp
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
else
    RES=`cat ${PLAN}`
    if [ "$RES" != "$EXPECTED" ]; then
        echo "Plan result did not equal the expected."
        exit 1
    fi
    compare_trees
fi

echo "Test SHOP with 1 argument and plan tree"
INPUT=$(mktemp -t "inputXXXXXX")
cat logistic.lisp > ${INPUT}
cat Log_ran_problems_15.lisp >> ${INPUT}
PLAN=$(mktemp -t "planXXXXXX")
PLAN_TREE=$(mktemp -t "treeXXXXXX")
shop --tree-file ${PLAN_TREE} --plan-file ${PLAN} ${INPUT}
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
else
    RES=`cat ${PLAN}`
    if [ "$RES" != "$EXPECTED" ]; then
        echo "Plan result did not equal the expected."
        exit 1
    fi
    compare_trees
fi


echo "Test ESS-SHOP with 2 arguments"

RES=$(ess-shop logistic.lisp Log_ran_problems_15.lisp | shop_plan_only)
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
elif [ "$RES" != "$EXPECTED" ]; then
    echo "Plan result did not equal the expected."
    exit 1
fi

echo "Test ESS-SHOP with 1 argument"
INPUT=$(mktemp -t "input.XXXXXXXXXXX")
cat logistic.lisp > ${INPUT}
cat Log_ran_problems_15.lisp >> ${INPUT}
RES=$(ess-shop ${INPUT} | shop_plan_only)
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
elif [ "$RES" != "$EXPECTED" ]; then
    echo "Plan result did not equal the expected."
    exit 1
fi

echo "Test ESS SHOP with 2 arguments and plan-tree"

PLAN=$(mktemp -t "planXXXXXX")
PLAN_TREE=$(mktemp -t "treeXXXXXX")
ess-shop --tree-file ${PLAN_TREE} --plan-file ${PLAN} logistic.lisp Log_ran_problems_15.lisp
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
else
    RES=`cat ${PLAN}`
    if [ "$RES" != "$EXPECTED" ]; then
        echo "Plan result did not equal the expected."
        exit 1
    fi
    # These don't compare properly because the ESS trees have INOP nodes in them
    # compare_trees
fi

echo "Test ESS SHOP with 1 argument and plan tree"
INPUT=$(mktemp -t "inputXXXXXX")
cat logistic.lisp > ${INPUT}
cat Log_ran_problems_15.lisp >> ${INPUT}
PLAN=$(mktemp -t "planXXXXXX")
PLAN_TREE=$(mktemp -t "treeXXXXXX")
ess-shop --tree-file ${PLAN_TREE} --plan-file ${PLAN} ${INPUT}
EC=$?
if [ "$EC" -ne 0 ]; then
    echo "Failed to run planner successfully";
    exit $EC
else
    RES=`cat ${PLAN}`
    if [ "$RES" != "$EXPECTED" ]; then
        echo "Plan result did not equal the expected."
        exit 1
    fi
    # These don't compare properly because the ESS trees have INOP nodes in them
    # compare_trees
fi


exit 0
