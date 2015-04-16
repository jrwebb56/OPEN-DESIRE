
#!/bin/bash
DIRECTORY=~/desire/SYSPIC.lst
echo "/NEW" > $DIRECTORY
echo -n "/PIC '" >> $DIRECTORY
echo "$1'" >>$DIRECTORY
echo "/" >> $DIRECTORY
cat $1 >> $DIRECTORY
echo "/" >> $DIRECTORY
kwrite ~/desire/SYSPIC.lst



