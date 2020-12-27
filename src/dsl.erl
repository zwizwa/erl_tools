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
              ({Pid, {op, Op, Arg}}, State = #{ ops := Ops }) ->
                   {Val, State1} = Ops(Op, Arg, State),
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
            {ok, {Value, State}}
    end.

op(Op, Args) ->
    obj:call(get(dsl_state), {op, Op, Args}).


%% Simple dataflow language.
compile_dataflow(Program, ProgramArgs) ->
    compile_dataflow(Program, ProgramArgs, #{}).
compile_dataflow(Program, ProgramArgs, Config) ->
    DefaultState = #{
      env => [],
      ops =>
          fun(Op, Args, State = #{ env := Env}) ->
                  %% Instances are counted per operation type.
                  OpType = optype(Op),
                  N = maps:get({next, OpType}, State, 0),
                  Bind = maps:get(bind, State, fun bind/4),
                  {Node,Env1} = Bind({OpType, N}, Op, Args, Env),
                  State1 = 
                      maps:merge(
                        State,
                        #{{next, OpType} => N + 1,
                          env => Env1}),
                  {Node, State1}
          end
     },
    InitState = maps:merge(DefaultState, Config),
    {ok, {Output,State}} =
        eval(InitState, Program, ProgramArgs),
    %% Provide bindings in intantiation order.
    Bindings = lists:reverse(maps:get(env, State)),
    {Output, Bindings}.

%% Default evaluator/binder is just compilation to syntax data
%% structure.  For an example where this is overridden, see
%% epid_app.erl which performs processor instantiation eagerly.
bind({OpType, N}, Op, Args, Env) ->
    Node = {node, {OpType, N}},
    Binding = {Node, {op, {Op, Args}}},
    Env1 = [Binding|Env],
    {Node, Env1}.

optype(Op) ->
    case Op of
        {OpT,_} -> OpT;
        _       -> Op
    end.


example() ->
    compile_dataflow(
      fun(A, B) ->
              %% Typically these wo3uld be wrapped in an interface
              %% function, but that is not necessary for testing.
              C = op(mul, [A,A]),
              D = op(mul, [B,B]),
              op(add, [C, D])
      end,
      [a, b]).
     


      
