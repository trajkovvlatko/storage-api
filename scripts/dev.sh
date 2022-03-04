#!/bin/bash
stack build --ghc-options="-Wall -Werror" --fast --file-watch --exec "./scripts/killRun.sh"
