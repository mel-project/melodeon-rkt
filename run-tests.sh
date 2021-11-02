#!/bin/sh
set -e
raco make -v *-test.rkt
raco make -v type-sys/*-test.rkt
raco make -v grammar/*-test.rkt
for file in *-test.rkt type-sys/*-test.rkt grammar/*-test.rkt
do

racket "$file" >&1
#if [ "$(racket "$file" >&1)" ]; then
  #echo "Encountered an error in $file"
  #exit 1
#else
#  echo "No error"
#fi

done
echo "Done testing"
