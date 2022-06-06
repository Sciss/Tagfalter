#!/bin/bash
cd "$(dirname "$0")" # go into directory of script

UNIT="1"
if [ ! -z "$1" ]; then
  UNIT="$1"
fi

echo "target: $UNIT"

scp ../Tagfalter.jar pi@klangpi0$UNIT.local:Documents/projects/Klangnetze/
