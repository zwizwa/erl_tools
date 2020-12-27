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
         compile_dataflow/2,
         compile_dataflow/3,
         example/0]).
eval(InitState, Function, Arguments) ->
    StatePid = 
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
                  put(dsl_state, StatePid),
                  Value = apply(Function, Arguments),
                  State = obj:call(StatePid, dump),
                  Pid ! {Ref, Value, State}
          end),
    receive
        {Ref, Value, State} ->
            exit(StatePid, normal),
            {ok, #{ value => Value, state => State}}
    end.

op(Op, Args) ->
    obj:call(get(dsl_state), {op, Op, Args}).


%% Simple dataflow language.
compile_dataflow(Program, ProgramArgs) ->
    compile_dataflow(Program, ProgramArgs, #{}).
compile_dataflow(Program, ProgramArgs, Config) ->
    DefaultState = #{
      env => [],
      bind =>
          %% Perform instance allocation, and call into bind-like
          %% instantiator.
          fun(OpType, Args, State = #{ bind_dfl := Bind }) ->
                  N = maps:get({next, OpType}, State, 0),
                  InstanceId = {OpType, N},
                  Bind(InstanceId, Args,
                       maps:put({next, OpType}, N + 1, State))
          end
     },
    InitState = maps:merge(DefaultState, Config),
    eval(InitState, Program, ProgramArgs).



%% EXAMPLE

%% Binder for compiling to syntax data structure.
bind_compile({OpType, N}, Args, State = #{ env := Env }) ->
    Node = {node, {OpType, N}},
    Binding = {Node, {op, {OpType, Args}}},
    Env1 = [Binding|Env],
    {Node, maps:put(env, Env1, State)}.

example() ->
    compile_dataflow(
      fun(A, B) ->
              %% Typically these wo3uld be wrapped in an interface
              %% function, but that is not necessary for testing.
              C = op(mul, [A,A]),
              D = op(mul, [B,B]),
              op(add, [C, D])
      end,
      [a, b],
      #{ bind_dfl => fun bind_compile/3 }).
     


      
