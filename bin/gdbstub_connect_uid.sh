#!/bin/bash
set -e
[ -z "$1" ] && echo "usage: $0 <uid>" && exit 1
HERE=$(dirname $0)
exec $HERE/gdbstub_connect.dynamic.host.elf $($HERE/uid_to_tty.sh "$1")

