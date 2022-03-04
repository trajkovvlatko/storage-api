#!/bin/bash
stack build --ghc-options="-Wall -Werror -Wwarn=incomplete-patterns" --fast --file-watch --exec "./scripts/killRun.sh"
