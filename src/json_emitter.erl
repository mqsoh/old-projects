-module(json_emitter).
-export([
    term/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


term(true) -> "true";
term(false) -> "false";
term(null) -> "null";
term(nil) -> "null";

term(Term) when is_float(Term) ->
    % Float_to_list is pretty janky:
    %     > float_to_list(1.12345).
    %     "1.12345000000000005969e+00"
    lists:flatten(io_lib:format("~g", [Term]));

term(Term) when is_integer(Term) ->
    integer_to_list(Term);

term(Term) when is_atom(Term) ->
    string:join(["\"", atom_to_list(Term), "\""], "");

term(Term) when is_map(Term) ->
    object(Term);

term(Term) when is_tuple(Term) ->
    term(tuple_to_list(Term));

term(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> string(Term, []);
        false -> array(Term)
    end.


string(String, []) ->
    string(String, [$"]);
string([], Acc) ->
    lists:reverse([$" | Acc]);

% Escape.
string([$" | Rest], Acc) -> string(Rest, [$", $\\ | Acc]);
string([$\\ | Rest], Acc) -> string(Rest, [$\\, $\\ | Acc]);
string([$/ | Rest], Acc) -> string(Rest, [$/, $\\ | Acc]);
string([$\b | Rest], Acc) -> string(Rest, [$b, $\\ | Acc]);
string([$\f | Rest], Acc) -> string(Rest, [$f, $\\ | Acc]);
string([$\n | Rest], Acc) -> string(Rest, [$n, $\\ | Acc]);
string([$\r | Rest], Acc) -> string(Rest, [$r, $\\ | Acc]);
string([$\t | Rest], Acc) -> string(Rest, [$t, $\\ | Acc]);

string([Char | Rest], Acc) ->
    string(Rest, [Char | Acc]).


object(Map) ->
    object(Map, maps:keys(Map), []).

object(_Map, [], Acc) ->
    "{" ++ string:join(Acc, ", ") ++ "}";
object(Map, [Key | Keys], Acc) ->
    object(Map, Keys, [string:join([term(Key), term(maps:get(Key, Map))], ": ") | Acc]).


array(Term) ->
    array(Term, []).

array([], Acc) ->
    "[" ++ string:join(lists:reverse(Acc), ", ") ++ "]";
array([Head | Tail], Acc) ->
    array(Tail, [term(Head) | Acc]).


-ifdef(TEST).

term_test() ->
    "true" = term(true),
    "false" = term(false),
    "null" = term(null),
    "null" = term(nil),
    "\"foobar\"" = term(foobar),
    "\"￦𝄞\"" = term("￦𝄞"),
    "\"  \\\" \\\\ \\/ \\b \\f \\n \\r \\t  \"" = term("  \" \\ / \b \f \n \r \t  "),
    "{\"foo\": \"bar\", \"baz\": \"buzz\"}" = term(#{foo => bar, baz => buzz}),
    "{\"foo\": \"bar\", \"baz\": [\"b\", \"u\", \"zz\"]}" = term(#{foo => bar, baz => [b, u, zz]}),
    "[true, false, null, null]" = term([true, false, null, nil]),
    % This output is idiosyncratic to the io_lib:format specification. I think
    % that not-wrong is the best I can hope for right now.
    "[0.123400, 1.23400, 1.23400e-2, 0, -1, 1, 1.23400e+4, 123.400]" = term([0.1234, 0.1234e1, 0.1234e-1, 0, -1, 1, 1234.0e1, 1234.0e-1]),
    ok.

-endif.
