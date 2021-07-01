#!/bin/bash
TEST_DIR=ILTP
PROLOGPGM=fCube/fCube
for i in $TEST_DIR/SYJ201* $TEST_DIR/SYJ202*00{1,2,3,4,5,6,7,8}* $TEST_DIR/SYJ20{3,4,5,6,7,8,9}*  $TEST_DIR/SYJ21{0,1,2}*
do
#NAME=`basename $i`
#echo $NAME 
echo `swipl -G1g -s $PROLOGPGM < $i 2>&1 | egrep 'inferences|search'` | awk '{print $13, "Time:", $4, $14, $15,$16, $17}' 
done
