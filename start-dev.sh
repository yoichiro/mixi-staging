#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname mixi_staging_dev \
    -s mixi_staging \
    -s reloader
