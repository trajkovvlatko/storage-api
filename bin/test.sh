#!/bin/bash
ENV=test stack test --fast --file-watch --exec "./bin/restart.sh"
