#!/bin/bash
SOURCE=$1
DESTINATION=$2
PGM_PROLOG=$3
for i in $SOURCE/*
do
NAME=`basename $i`
echo $i
swipl -s ${PGM_PROLOG} < $i >  ${DESTINATION}/${NAME}
done


