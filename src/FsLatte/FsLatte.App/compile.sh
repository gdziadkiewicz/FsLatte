#!/bin/bash
outputBC="${1%.ll}.bc"
output="${1%.ll}.cmpout"
llvm-as $1 &>$output
llvm-link -o $outputBC $outputBC $2 $3 &>>$output