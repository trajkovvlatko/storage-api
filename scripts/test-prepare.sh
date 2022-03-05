#!/bin/bash
source .env
dropdb $PG_TEST_DB
createdb $PG_TEST_DB
pg_dump $PG_DB --schema-only > ./test/db.sql
psql $PG_TEST_DB < ./test/db.sql
