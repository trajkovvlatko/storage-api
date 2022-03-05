#!/bin/bash
ENV=dev stack build --fast --file-watch --exec "./scripts/killRun.sh"

# ENV=dev stack build --ghc-options="-Wall -Werror -Wwarn=incomplete-patterns" --fast --file-watch --exec "./scripts/killRun.sh"
