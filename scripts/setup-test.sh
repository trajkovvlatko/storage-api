#!/bin/bash
source .env
dropdb $PG_TEST_DB
createdb $PG_TEST_DB
pg_dump $PG_DB --schema-only > ./db.sql
psql $PG_TEST_DB < ./db.sql
