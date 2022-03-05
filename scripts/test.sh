#!/bin/bash
ENV=test stack test --fast --file-watch --exec "./scripts/killRun.sh"
