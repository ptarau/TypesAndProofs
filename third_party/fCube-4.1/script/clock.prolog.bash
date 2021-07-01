#!/bin/bash
PROVER=$1 
TEST_DIR=$2
MINUTI_A_DISPOSIZIONE=9  #Nota: MINUTI_A_DISPOSIZIONE=0, significa avere 1 minuto, 
                         #      MINUTI_A_DISPOSIZIONE=1, significa avere 2 minuti, etc.
                         #Attenzione: con 59 la condizione del while e' sempre  vera

for i in $TEST_DIR/SYJ201*01{8,9}* $TEST_DIR/SYJ201*02* $TEST_DIR/SYJ202*00{6,7,8}*  $TEST_DIR/SYJ206*01{8,9}* $TEST_DIR/SYJ206*02* $TEST_DIR/SYJ207*01{8,9}* $TEST_DIR/SYJ207*02* $TEST_DIR/SYJ208*01{5,6,7}* $TEST_DIR/SYJ209* $TEST_DIR/SYJ21{0,1,2}*
do
swipl -G1g  -s $PROVER < $i  > output.$$ 2>&1 &

LASTBGPROCESS=$!
  
#calcola da quanti minuti di cpu e' in esecuzione
MINUTI=`ps ww | awk '{ print $1, $4}' | grep "^\<$LASTBGPROCESS\>" | awk '{print $2}'| awk --field-separator ":" '{print $1} '`
  
#esce se si supera il limite di minuti o se il processo termina 
while [ ${MINUTI:=-1} -le $MINUTI_A_DISPOSIZIONE -a ${MINUTI:=-1} -ne -1 ] 
    do 
  MINUTI=`ps ww | awk '{ print $1, $4}' | grep "^\<$LASTBGPROCESS\>" | awk '{print $2}'| awk --field-separator ":" '{print $1} '` 
done
  
#se la varaibile MINUTI vale -1 il processo ha terminato prima del limite (vedi comando ps | grep sopra)
if [ $MINUTI -eq -1 ]  
    then 
    echo "$i  " `cat output.$$ | egrep "search|CPU" ` " ; " 
#|  awk '{print $1,$2,$6,$11}' #stampa output del prover
else  
    /bin/kill -9 $!  2> /dev/null #termina il processo
    echo  "$i $i  unknown  timeout "
fi

done

