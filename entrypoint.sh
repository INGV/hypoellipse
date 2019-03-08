#!/bin/bash

echo Your container args are: "$@"

# docker run -v $(pwd)/example:/opt/data hyp2000 italy2000.hyp

DIR_DATA=/opt/data

if [ -d ${DIR_DATA} ]; then
	
	# 
		if [ -f ${DIR_DATA}/${1} ]; then
			FILE_INPUT=${1}
			cd ${DIR_DATA}
			/opt/hypoellipse//hypoellipse < ${FILE_INPUT}
		else
			echo " The \"${DIR_DATA}/${1}\" doesn't exist."
			echo ""
			exit 1
		fi
else
	echo " the \"${DIR_DATA}\" doesn't exist."
        echo ""
        exit 1
fi

