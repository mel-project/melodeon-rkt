#!/bin/sh
set -e
raco make -v *-test.rkt
raco make -v type-sys/*-test.rkt
raco make -v grammar/*-test.rkt
for f in *-test.rkt type-sys/*-test.rkt grammar/*-test.rkt
do
racket $f
done
echo "Done testing"
