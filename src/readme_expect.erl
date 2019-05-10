%% This is an example of a host module for an .expect file.

%% An expect file is an Erlang code fragment containing pairs of
%% Erlang functions and the values they are expected to evaluate to.

%% This module sets up the infrastructure to make this all work, such
%% that when the .expect file is saved in Emacs, it will be compiled,
%% uploaded to an Erlang VM, evaluated, and the Emacs window is
%% updated with the results of the evaluation.

%% Expect files can serve as REPL "notebooks", low overhead unit
%% tests, or example code to accompany documentation.  They can be
%% combined with version control to show how the behavior of code
%% changes over time.  Expect files can capture interactive REPL tests
%% that would be performed anyway at the Erlang console during
%% development, and collect them for later reference.

%% The basic idea of expect tests is explained here in detail:
%% https://blog.janestreet.com/testing-with-expectations

-module(readme_expect).
-compile(export_all).

%% Tests are split up in two parts: an .expect file and an Erlang
%% module that hosts the code in the file (this module).  The expect
%% file is included in the module to expose its entry point as a
%% function.  See readme_expect_runt/0 in this file:
-include("../test/readme_expect.expect").

%% To execute the test, we pass that function and the source location
%% of the module to expect:run_form/2, which will then produce an
%% .expect.new file that contains the new test results.
expect_file() ->
    filename:dirname(?FILE) ++ 
        "/../test/readme_expect.expect".
expect_test() ->
    expect:run_form(
      expect_file(),
      fun readme_expect_run/0).

%% The module can be used to define functions used in the tests.
add_one(X) ->
    X+1.


%% Note that the .expect format does not support Erlang comments.  Use
%% strings instead.

