redo-ifchange $2
# The tool that runs the top level redo can define this to deploy.
if [ ! -z "$INSTALL_CMD" ]; then $INSTALL_CMD $2 >&2; fi
touch $3
