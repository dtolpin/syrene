#!/bin/sh

SYRENE=/home/dvd/work/Ds/SYRENE/

PATH_INFO=${PATH_INFO:-/anna/0}
export PATH_INFO

runhugs -98 -P$SYRENE/src: $SYRENE/src/qre.lhs $SYRENE/data/
