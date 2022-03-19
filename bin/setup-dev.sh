#!/bin/bash
source .env
dropdb $PG_DB
createdb $PG_DB
psql $PG_DB < ./db.sql
