#!/bin/sh

# get ADDRESS_GARDEN
source /home/aws/.bash_profile

DATA_FOLDER="/home/aws/logger_data/"
LOG_FILE="/home/aws/read_store.log"
BOARD_FILE="/home/aws/board.csv"
BOARD_FILE_TEMP="/home/aws/board_temp.csv"
WEBPAGE_FOLDER="/home/aws/webpage_simple/"

# times are UTC throughout the script

# current time in seconds after epoch
CURRENT_TIME_sepoch=$(date +%s)
# current time standard format
CURRENT_TIME=$(date -u --date="@${CURRENT_TIME_sepoch}" +"%F %T")
# current time without seconds
CURRENT_TIME_wo_s=$(date -u --date="@${CURRENT_TIME_sepoch}" +"%Y-%m-%d %H:%M")
# current time full string
CURRENT_TIME_full=$(date -u --date="@${CURRENT_TIME_sepoch}" +"%Y%m%d_%H%M%S")

DATA_FILE_GARDEN=${DATA_FOLDER}/${CURRENT_TIME_full}_garden.csv
DATA_FILE_ROOF=${DATA_FOLDER}/${CURRENT_TIME_full}_roof.csv

#echo ${CURRENT_TIME}
#echo ${CURRENT_TIME_wo_s}
#echo ${CURRENT_TIME_full}

# set logger to current time
pycr1000 settime ${ADDRESS_GARDEN} "${CURRENT_TIME}" &
pycr1000 settime ${ADDRESS_ROOF}   "${CURRENT_TIME}"

if [ -f "last_access" ]; then
    LAST_ACCESS=$(cat last_access)
else 
    # fake date
    LAST_ACCESS="2000-01-01 00:00"
fi

#echo $LAST_ACCESS

# get data
pycr1000 getdata --start "${LAST_ACCESS}" --stop "${CURRENT_TIME_wo_s}" ${ADDRESS_GARDEN} 'Table1' ${DATA_FILE_GARDEN}
if [ $? -ne 0 ]; then
    exit 1
fi
pycr1000 getdata --start "${LAST_ACCESS}" --stop "${CURRENT_TIME_wo_s}" ${ADDRESS_ROOF}   'Table1' ${DATA_FILE_ROOF}
if [ $? -ne 0 ]; then
    exit 2
fi

#echo "${CURRENT_TIME_full}" >> ${LOG_FILE}
Rscript --vanilla store_logger_data.R ${DATA_FILE_GARDEN} ${DATA_FILE_ROOF} ${BOARD_FILE_TEMP} ${WEBPAGE_FOLDER} # >> ${LOG_FILE}
if [ $? -ne 0 ]; then
    exit 3
fi

echo "${CURRENT_TIME_wo_s}" > last_access

# convert to DOS end-of-line character
unix2dos -n ${BOARD_FILE_TEMP} ${BOARD_FILE}

# copy web pages on login node
scp -i ~/.ssh/id_rsa2 ${WEBPAGE_FOLDER}/aws_german.html login:/var/www/html/aws
scp -i ~/.ssh/id_rsa2 ${WEBPAGE_FOLDER}/aws_english.html login:/var/www/html/aws

xz ${DATA_FILE_GARDEN}
xz ${DATA_FILE_ROOF}
