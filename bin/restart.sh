#!/bin/bash
kill -9 `pgrep storage-api-exe`
stack exec storage-api-exe &
sleep 0.6
echo -e "server running with PID={`pgrep storage-api-exe`}"
