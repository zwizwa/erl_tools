#!/bin/bash
set -e
[ -z "$1" ] && echo "usage: $0 <uid>" && exit 1
cd /sys/class/tty
for d in ttyACM*; do
	echo "checking $d" >&2
	uid=$(cat $(readlink -f $d/device)/../serial | tr -d "\n")
	[ "$uid" == "$1" ] && echo "/dev/$d" && exit 0
	echo "uid=$uid"
done
echo "$1 not found" >&2
exit 1



