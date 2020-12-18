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
    InitState = #{
      env => [],
      ops =>
          fun(Op, Args, State = #{ env := Env}) ->
                  Node = {r, length(Env)},
                  Binding = {Node, {op, {Op, Args}}},
                  State1 = maps:put(env, [Binding | Env], State),
                  {Node, State1}
          end
     },
    {ok, {Output,State}} =
        eval(InitState, Program, ProgramArgs),
    %% Provide bindings in intantiation order.
    Bindings = lists:reverse(maps:get(env, State)),
    {Output, Bindings}.

example() ->
    compile_dataflow(
      fun(A, B) ->
              %% Typically these wo3uld be wrapped in an interface
              %% function, but that is not necessary for testing.
              op(mul, [op(add, [A, B]), A])
      end,
      [a, b]).
     


      
