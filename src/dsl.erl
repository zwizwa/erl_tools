%% Abstract Syntax

%% Here's the conundrum:
%%
%% - Passing state explicitly is a pain.
%%
%% - Explicit monad syntax is really annoying for a "pure" DSL.
%%
%% - Instantiating and using variables bound to context instead of
%%   just named Erlang functions is a pain.
%%
%% So, why not stop worring and use the process dictionary to bind an
%% evaluator process to its "accumulator process".  The only actual
%% effect is data structure compilation in the background,
%% unobservable to the pure DSL code.
%%
%% Note that we could put the compiler state in the process dictionary
%% as well, but that seems like more of a violation of principles.


-module(dsl).
-export([eval/3, handle/2, op/2, example/0]).
eval(InitState, Function, Argument) ->
    CompilerPid = 
        %% Compiler is a separate process to isolate side effects.
        serv:start(
          {handler,
           fun() -> InitState end,
           fun ?MODULE:handle/2}),
    Ref = erlang:make_ref(),
    Pid = self(),
    _EvalPid = 
        %% Isolate it in a separate process, because we use the
        %% process dictionary.
        spawn_link(
          fun() ->
                  put(dsl_compiler, CompilerPid),
                  Value = Function(Argument),
                  State = obj:call(CompilerPid, dump),
                  Pid ! {Ref, Value, State}
          end),
    receive
        {Ref, Value, State} ->
            exit(CompilerPid, normal),
            {ok, {Value, State}}
    end.

%% Keep this really simple.
handle({_, dump}=Msg, State) ->
    obj:handle(Msg, State);
handle({Pid, {op, Op, Arg}}, State = #{ ops := Ops }) ->
    {Val, State1} = Ops(Op, Arg, State),
    obj:reply(Pid, Val),
    State1.
op(Op, Arg) ->
    obj:call(get(dsl_compiler), {op, Op, Arg}).


%% Simple dataflow language.
compile_dataflow(Program) ->
    InitState = #{
      env => [],
      ops =>
          fun(Op, {_, _}=Args, State = #{ env := Env}) ->
                  Node = {r, length(Env)},
                  Binding = {Node, {Op, Args}},
                  State1 = maps:put(env, [Binding | Env], State),
                  {Node, State1}
          end
     },
    {ok, {Output,State}} =
        eval(InitState, Program, {a, b}),
    Dag = maps:from_list(maps:get(env, State)),
    {Output,Dag}.

example() ->
    compile_dataflow(
        fun({A, B}) ->
                %% Typically these would be wrapped in an interface
                %% function, but that is not necessary for testing.
                op(mul,
                   {op(add, {A, B}),
                    A})
        end).
      
