#!/usr/bin/env bash
sqlite3 ~/.wunderlist-to-taskwarrior/db.sqlite3 < ./database/create-indexes.sql
