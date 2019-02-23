# Allow for build system to provide a wrapper script for build tools.
# This is used by exo.
[ -z   "$TARGET_WRAP" ] && echo "no TARGET_WRAP variable" >&2 && exit 1
[ ! -f "$TARGET_WRAP" ] && echo "$TARGET_WRAP script not provided" >&2 && exit 1
[ ! -x "$TARGET_WRAP" ] && echo "$TARGET_WRAP script not executable" >&2 && exit 1

cat <<EOF >$3
. ./env.sh
GCC="$($TARGET_WRAP $2 gcc)"
EOF
redo-stamp <$3
