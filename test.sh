#!/bin/bash

# prepare db
psql -U postgres -w -h db -p 5432 postgres < db/migrations/20160501_create_todo_table.sql

# run test
cabal sandbox init && \
    cabal install --only-dependencies -j4 && \
    cabal configure --enable-tests && \
    cabal build && \
    cabal test --show-details=always
