raco make -v *-test.rkt
raco make -v type-sys/*-test.rkt
raco make -v grammar/*-test.rkt
racket *-test.rkt
racket type-sys/*-test.rkt
racket grammar/*-test.rkt
echo "Done testing"
