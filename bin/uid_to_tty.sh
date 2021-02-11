#!/bin/sh
# Copied from https://github.com/zwizwa/erl_tools/blob/master/bin/uid_to_tty.sh
set -e
[ -z "$1" ] && echo "usage: $0 <uid>" && exit 1
cd /sys/class/tty
for d in ttyACM*; do
	echo "checking $d" >&2
	uid=$(cat $(readlink -f $d/device)/../serial | tr -d "\n")
	[ "$uid" = "$1" ] && echo "/dev/$d" && exit 0
	echo "uid=$uid" >&2
done
echo "$1 not found" >&2
exit 1



