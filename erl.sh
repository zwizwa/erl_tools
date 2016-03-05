#!/bin/bash
exec erl \
     -pa ebin `find deps -name 'ebin'`

