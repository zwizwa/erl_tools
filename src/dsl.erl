%% TL;DR
%%
%% 1. Implement a DSL piggy-backing on Erlang evaluation order, with
%%    function application abstracted as dsl:op/2
%%
%% 2. An Erlang function containing a DSL program can be evaluated
%%    using eval/3, which spawns a process, stores the evaluator
%%    process in the process directory (variable 'dsl_state'), and
%%    evaluates the Erlang function.
%%
%% I believe this is related to 'effect handlers'.
%% Comment below is older.


%% A "Final" higher order abstract syntax representation in Erlang.
%% http://okmij.org/ftp/tagless-final/index.html
%% https://en.wikipedia.org/wiki/Higher-order_abstract_syntax
%% 
%% The main purpose of this abstraction is to implement the compiler
%% for a pure dataflow language.
%%
%% The side effect needed to thread compilation state through the
%% evaluation of the abstract syntax is implemented using a second
%% process.  The individual operators that implement language
%% semantics resemble operators in a State Monad.  The evaluaor
%% process is implictly linked to the evaluator process through the
%% process dictionary, and state-threading operations are implemented
%% using RPC.  Note that this is impure code, but the side effect is
%% only observable during the extent of eval/3, which is reasonable.

-module(dsl).
-export([eval/3,
         op/2,
         compile_dataflow/3,
         example/0]).

%% DSL programs are implemented in terms of dsl:op/2 The link to the
%% compiler (providing language semantics) is hidden in a process
%% variable.
op(Op, Args) ->
    CompilerPid = get(dsl_state),
    obj:call(CompilerPid, {op, Op, Args}).

%% The evaluator instantiates a compiler, then evaluates the DSL
%% (encoded as an Erlang function) in a context that has a reference
%% to the compiler process, via the 'dsl_state' process variable.
eval(InitState, DSLFun, DSLArguments) ->
    CompilerPid = 
        %% Compiler is a separate process to isolate side effects.
        serv:start(
          {handler,
           fun() -> InitState end,
           %% Keep this really simple.
           fun({_, dump}=Msg, State) ->
                   obj:handle(Msg, State);
              ({Pid, {op, Op, Arg}}, State = #{ bind := Bind }) ->
                   {Val, State1} = Bind(Op, Arg, State),
                   obj:reply(Pid, Val),
                   State1
           end}),
    Ref = erlang:make_ref(),
    Pid = self(),
    _EvalPid = 
        %% Isolate it in a separate process, because we use the
        %% process dictionary.
        spawn_link(
          fun() ->
                  put(dsl_state, CompilerPid),
                  Value = apply(DSLFun, DSLArguments),
                  State = obj:call(CompilerPid, dump),
                  Pid ! {Ref, Value, State}
          end),
    receive
        {Ref, Value, State} ->
            exit(CompilerPid, normal),
            {ok, #{ value => Value, state => State}}
    end.



%% EXAMPLES

%% Note that epid_app doesn't use any of these.  It uses raw bind,
%% relying on memoized instantiation.

%% Dataflow language with instance ID allocator.
compile_dataflow(Config, Program, ProgramArgs) ->
    DefaultState = #{ bind => fun bind_alloc/3 },
    InitState = maps:merge(DefaultState, Config),
    eval(InitState, Program, ProgramArgs).
%% Perform instance ID allocation, and call into specialized
%% binding/instantiation operation.
bind_alloc(OpType, Args, State = #{ bind_instance := Bind }) ->
    N = maps:get({count, OpType}, State, 0),
    InstanceId = {OpType, N},
    Bind(InstanceId, Args,
         maps:put({count, OpType}, N + 1, State)).

%% ... specialized to compile to concrete syntax.
compile_dataflow(Program, ProgramArgs) ->
    Config = #{ env => [], bind_instance => fun bind_compile/3 },
    compile_dataflow(Config, Program, ProgramArgs).
bind_compile({OpType, N}, Args, State = #{ env := Env }) ->
    Node = {node, {OpType, N}},
    Binding = {Node, {op, {OpType, Args}}},
    Env1 = [Binding|Env],
    {Node, maps:put(env, Env1, State)}.

%% Binder for compiling to syntax data structure.
example() ->
    compile_dataflow(
      fun(A, B) ->
              C = op(mul, [A, A]),
              D = op(mul, [B, B]),
              op(add, [C, D])
      end,
      [a, b]).
