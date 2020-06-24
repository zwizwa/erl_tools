-module(task_queue).
-export([handle/2]).

%% A mixin task queue for serializing operations.  This does not need
%% initialization.  It will use the key 'task_queue' in the State
%% dictionary.

%% About mixins: sometimes it is easier to just compose handler
%% functions that keep some state in the main process dictionary, as
%% opposed to using separate objects with message protocol and process
%% management.

%% Use the sequential nature of a server process to implement a
%% sequential task queue.  The operations themselves are performed in
%% an isolated process, wrapped in a catch-all error handler.

%% This was originally written to make bluepill:need/1 sequential, as
%% it is called in parallel from the parallel build system, which
%% creates a race condition.

handle({_Pid, {task, _Task}}=PT, State) ->
    Hub = self(),
    Queue = maps:get(task_queue, State, []),
    case Queue of
        [] ->
            %% Nothing is currently running.  Start the task.
            Hub ! task_next;
        [_|_] ->
            %% Something is running.  End of current task will start
            %% the next one.
            ok
    end,
    maps:put(task_queue, Queue ++ [PT], State);

%% Start a task.  This will result in task_done message.
handle(task_next, State) ->
    Hub = self(),
    Queue = maps:get(task_queue, State, []),
    case Queue of
        [] ->
            ok;
        [{_Pid, {task, Task}} | _] ->
            spawn_link(
              fun() ->
                      log:info("task: ~p~n", [Task]),
                      Rv =
                          try {ok, Task(Hub)}
                          catch C:E -> {error,{C,E,erlang:get_stacktrace()}}
                          end,
                      Hub ! {task_done, Rv}
              end)
    end,
    State;

%% Handle finishing of task.
handle({task_done, Rv}, State) ->
    Queue = maps:get(task_queue, State, []),
    case Queue of
        [] ->
            %% This is an error
            throw(task_done_empty_queue);
        [{Pid, {task, _Task}} | QueueTail] ->
            obj:reply(Pid, Rv),
            ?MODULE:handle(
              task_next,
              maps:put(task_queue, QueueTail, State))
    end.

