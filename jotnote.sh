#!/bin/sh

JOTNOTE_FILE="${JOTNOTE_FILE:-${HOME}/.jotnote}"
touch "${JOTNOTE_FILE}"

case "$1" in
    "")
        cat "${JOTNOTE_FILE}"
        ;;
    *)
        echo -n "$(date --iso-8601=seconds)   " >> "${JOTNOTE_FILE}"
        echo "$(pwd)" >> "${JOTNOTE_FILE}"
        echo "   $@" >> "${JOTNOTE_FILE}"
        echo >> "${JOTNOTE_FILE}"

        echo
        tail -n3 "${JOTNOTE_FILE}" | head -n2 | wrapnote
        ;;
esac
