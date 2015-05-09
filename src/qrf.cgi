#!/bin/sh

SYRENE=/Users/dvd/Work/Ds/SYRENE/
#QRF="/usr/local/bin/runhugs -98 +o -P$SYRENE/src: $SYRENE/src/qrf.lhs"
QRF="$SYRENE/src/qrf"

PATH_INFO=${PATH_INFO:-/anna/0}
export PATH_INFO

$QRF $SYRENE/
