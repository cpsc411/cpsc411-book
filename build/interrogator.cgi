#!/bin/bash

#exec racket -l cpsc411/interrogator/interrogator-cgi.rkt
# NOTE workaround some weird collection path bug by manually specifying path to Racket
exec /cs/local/lib/pkg/racket-8.3/bin/racket interrogator-cgi.rkt
