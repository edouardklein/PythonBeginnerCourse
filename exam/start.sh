#!/usr/bin/env bash
set -euxo pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"/"
ASDF_OUTPUT_TRANSLATIONS="/:$HOME/.cache/common-lisp/my-dir/" rlwrap sbcl \
    --load $GUIX_ENVIRONMENT/share/common-lisp/source/asdf/asdf.lisp \
    --eval  '(setf asdf:*central-registry* '"'"'(#P"'$SCRIPT_DIR'"))' \
    --eval '(asdf:operate '"'"'asdf:load-op :exam)' \
    --load "$(find -L $GUIX_ENVIRONMENT  -name 'swank-loader.lisp')" \
    --eval '(swank-loader:init)' \
    --eval '(swank:create-server)' \
    --eval '(exam:start)'
