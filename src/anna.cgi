#!/bin/sh

SYRENE=/Users/dvd/work/Ds/SYRENE/
#IDX="runhugs -98 -P$SYRENE/src: $SYRENE/src/idx.lhs"
IDX="$SYRENE/src/idx"

PATH_INFO=${PATH_INFO:-/}
export PATH_INFO

$IDX "XEP support cases" \
     "http://bugzilla.renderx.com:8000/show_bug.cgi?id=" \
     "qrf.cgi/anna/" \
     "$SYRENE/data/anna/cases.csv" \
     "$SYRENE/net/anna.synet"
