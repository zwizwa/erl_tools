%%-define(RELOAD_TIMEOUT,infinity).  %% Production: turn it off?

%% FIXME: make sure all processes use the reloader facility
-define(RELOAD_TIMEOUT,1000). %% Debugging, check once per second.
-define(GDB_ARM,"../../gdb/gdb-mi").
-define(GDB_386,"gdb -i=mi").
