#!/bin/bash
outputBC="${1%.bc}.out"
lli $1 > $outputBC