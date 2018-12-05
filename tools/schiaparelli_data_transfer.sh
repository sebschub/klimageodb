#!/bin/bash

# get passwords and addresses
source /home/aws/.bash_profile

CURRENT_TIME=$(date -u +"%Y%m%d_%H%M%S")

DATA_FOLDER="/home/aws/schiaparelli/data_files/"

for tab in Table1 Table2; do
    sshpass -p ${PASS_PATAGONIA} scp ${USER_PATAGONIA}@${URL_PATAGONIA}:/home/aachen/CR800Series_2_${tab}* \
	${DATA_FOLDER}/${CURRENT_TIME}_${tab}_schiaparelli.csv
    if [ $? -ne 0 ]; then
	exit 1
    fi
done

Rscript --vanilla /home/aws/schiaparelli/dbAddData_schiaparelli.R ${DATA_FOLDER}/${CURRENT_TIME}_Table1_schiaparelli.csv

for tab in Table1 Table2; do
    xz ${DATA_FOLDER}/${CURRENT_TIME}_${tab}_schiaparelli.csv
done
