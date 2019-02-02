-module(pipeline).
-export([test/0]).

%% Basic observation: Erlang's message queue can serve as a buffer to
%% allow constructiong data transfer/processing pipelines.  However:
%% there is no backpressure.  Consider 2 extremes:
%%
%% - RPC: Request, response.
%% - Un-constrained flow
%%
%% The midpoint is to allow for a number of data packets to be in
%% flight such that buffers do not overflow.  It seems simplest to
%% write this as an RPC that is "out of phase".  E.g. send out n
%% requests to fill the pipeline, and from there on, send an ack for
%% each packet that makes it out the other end.
%%
%% How to translate this into an abstraction?  It seems simplest to
%% express it in terms of a message protocol.  Could be done in
%% obj.erl maybe, as an extension of obj:call?

test() ->
    ok.

