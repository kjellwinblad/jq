#!/usr/bin/env bash

grep '.*define.*(.*VERSION' "$1"/src/jq_nif.erl | sed -e 's/.*VERSION.*,.*\([[:digit:]]\+\).*/\1/'
