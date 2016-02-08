#!/bin/bash

PREV_FILES="";

while true
do
    FILES=$(find . -type f \
                   | egrep '(\.hs|\.cabal)$' \
                   | egrep -v '(\.stack|#)' \
                   | xargs md5sum)

    if [ "$FILES" != "$PREV_FILES" ]
    then
        PREV_FILES="$FILES";
        make test
    fi
    sleep 1;
    inotifywait -q -q -e close_write,moved_to,create src test app
done
