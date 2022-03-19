#!/bin/bash
ENV=dev stack build --fast --file-watch --exec "./scripts/restart.sh"
