#!/usr/bin/env bash
stack haddock
docs_at="$(stack path --local-install-root)/doc/"
S=$?
if [ "${S}" -eq "0" ]; then
    cd "$docs_at"
    DDIR="${1}-${2}-docs"
    cp -r "${1}-${2}" "${DDIR}" && tar -c -v -z --format=ustar -f "${DDIR}.tar.gz" "${DDIR}"
    CS=$?
    if [ "${CS}" -eq "0" ]; then
        echo "Uploading to Hackage…"
        curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@${DDIR}.tar.gz" "https://${3}:${4}@hackage.haskell.org/package/${1}-${2}/docs"
        exit $?
    else
        echo "Error when packaging the documentation"
        exit $CS
    fi
else
    echo "Error when trying to build the package."
    exit $S
fi
