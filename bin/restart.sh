#!/bin/bash
kill -9 `pgrep web-exe`
stack exec web-exe &
sleep 0.6
echo -e "server running with PID={`pgrep web-exe`} (if that's empty, then the server is not running)"