#!/bin/bash

#exec racket -l cpsc411/interrogator/lang-differ-cgi
# NOTE workaround some weird collection path bug by manually specifying path to Racket
exec ~/racket/racket/bin/racket -l cpsc411/interrogator/lang-differ-cgi
