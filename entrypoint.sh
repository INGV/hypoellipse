#!/bin/bash

echo Your container args are: "$@"

# docker run -v $(pwd)/example:/opt/data hyp2000 italy2000.hyp

DIR_DATA=/opt/data

if [ -d ${DIR_DATA} ]; then
	
	# 
	if [ -d ${DIR_DATA}/output ]; then
		DIR_OUTPUT=${DIR_DATA}/output
		rm ${DIR_OUTPUT}/*
	else 
		mkdir ${DIR_OUTPUT}/
	fi

	#
	if [ -d ${DIR_DATA}/input ]; then
		DIR_INPUT=${DIR_DATA}/input
		if [ -f ${DIR_INPUT}/${1} ]; then
			FILE_INPUT=${1}
			cd ${DIR_DATA}/input
			/opt/hypoellipse/hypoellipse < ${FILE_INPUT}
		else
			echo " The \"${DIR_INPUT}/${1}\" doesn't exist."
			echo ""
			exit 1
		fi
	else
		echo " the \"${DIR_DATA}/input\" doesn't exist."
		echo ""
		exit 1
	fi

else
	echo " the \"${DIR_DATA}\" doesn't exist."
        echo ""
        exit 1
fi

