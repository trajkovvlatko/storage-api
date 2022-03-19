#!/bin/bash
ENV=dev stack build --fast --file-watch --exec "./bin/restart.sh"
