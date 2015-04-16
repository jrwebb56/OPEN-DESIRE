
#!~/bin/bash
XFILE=~/desire/SYSPIC.lst
echo "/NEW" > $XFILE
echo -n "/PIC '" >> $XFILE
echo "$1'" >>$XFILE
echo "/" >> $XFILE
cat $1 >> $XFILE
echo "/" >> $XFILE
kwrite $XFILE



