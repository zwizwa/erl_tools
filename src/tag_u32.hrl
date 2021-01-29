
%% Main TAG_U32 type tag for uc_tools tagged packets.
-define(TAG_U32,16#FFF5).

%% The binary TAG_U32 protocll can be used in conjuction with a
%% convention to perform name resolution.  This allows protocols to be
%% defined symbolically, with a minimal set of hard-coded binary tags
%% that are used for name resolution.

%% If a path P supports name resolution, then it handles the RPC calls
%% addresses as P ++ [?TAG_U32_CTRL, CtrlTag],
-define(TAG_U32_CTRL,16#FFFFFFFF).
%% Where CtrlTag is one of these:
%%
%% Get name of P ++ [NumID] 
-define(TAG_U32_CTRL_ID_NAME,0).
%% Get type of P ++ [NumID]
-define(TAG_U32_CTRL_ID_TYPE,1).
%% Map symbolic name to NumId (symbolic directory lookup) 
-define(TAG_U32_CTRL_NAME_ID,2).

%% See tag_u32.erl for implementation of symbolic lookup.


%% Erlang-specific:
%%
%% Two form of RPC reply addresses are used.  One translates Erlang
%% pid binary representation to u32 path, the other keeps track of the
%% pid in a dictionary and uses a single integer.  ?INDIRECT is the
%% routing prefix for that integer.
-define(TAG_U32_INDIRECT,16#FFFFFE00).

