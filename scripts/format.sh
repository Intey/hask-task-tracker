#!/bin/sh
find src -iname '*.hs' | xargs floskell -c .floskell.json
