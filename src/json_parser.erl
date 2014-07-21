-module(json_parser).
-export([
    value/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


value([$ | Rest], Options) -> value(Rest, Options);
value([${ | Rest], Options) -> object(Rest, Options);
value([$[ | Rest], Options) -> array(Rest, Options);
value([$" | Rest], Options) -> string(Rest, Options);
value([$t, $r, $u, $e | Rest], _Options) -> {true, Rest};
value([$f, $a, $l, $s, $e | Rest], _Options) -> {false, Rest};
value([$n, $u, $l, $l | Rest], _Options) -> {null, Rest};
value(Rest, Options) -> number(Rest, Options).


object(Input, Options) -> object(Input, [], Options).

% Termination.
object([$} | Rest], Object_list, _Options) ->
    {maps:from_list(Object_list), Rest};
% Skip.
object([Char | Rest], Object_list, Options) when
    Char == $,;
    Char == $ ;
    Char == $\r;
    Char == $\n;
    Char == $\t ->
    object(Rest, Object_list, Options);
% Add property.
object([$" | Rest], Object_list, Options) ->
    {Key, Rest1} = begin
        {String_key, Rest1} = string(Rest, Options),
        case lists:member(atom_keys, Options) of
            true -> {list_to_atom(String_key), Rest1};
            _ -> {String_key, Rest1}
        end
    end,
    Rest2 = lists:foldl(
        fun (Unwanted, String) ->
            string:strip(String, left, Unwanted)
        end,
        Rest1,
        [$ , $:, $ ]),
    {Value, Rest3} = value(Rest2, Options),
    object(Rest3, [{Key, Value} | Object_list], Options).


array(Input, Options) -> array(Input, [], Options).

% Termination.
array([$] | Rest], Acc, _Options) ->
    {lists:reverse(Acc), Rest};
% Skip.
array([Char | Rest], Acc, Options) when
    Char == $,;
    Char == $ ;
    Char == $\r;
    Char == $\n;
    Char == $\t ->
    array(Rest, Acc, Options);
% Append.
array(Rest, Acc, Options) ->
    {Value, Rest1} = value(Rest, Options),
    array(Rest1, [Value | Acc], Options).


% Returns the input up until the next literal double quote, along with the
% rest of the input.
%
% string(Input) -> {Interpreted_string, Rest_of_input}.
%
% Input = string()
% Interpreted_string = string()
% Rest_of_input = string()
string(Input, _Options) ->
    string1(Input, []).

string1([], _Acc) ->
    error(unterminated_string);
% Escaped characters.
string1([$\\, $" | Rest], Acc) -> string1(Rest, ["\"", Acc]);
string1([$\\, $\ | Rest], Acc) -> string1(Rest, ["\\", Acc]);
string1([$\\, $/ | Rest], Acc) -> string1(Rest, ["/" | Acc]);
string1([$\\, $b | Rest], Acc) -> string1(Rest, ["\b" | Acc]);
string1([$\\, $f | Rest], Acc) -> string1(Rest, ["\f" | Acc]);
string1([$\\, $n | Rest], Acc) -> string1(Rest, ["\n" | Acc]);
string1([$\\, $r | Rest], Acc) -> string1(Rest, ["\r" | Acc]);
string1([$\\, $t | Rest], Acc) -> string1(Rest, ["\t" | Acc]);
% Unicode; UTF-16 surrogate pair.
string1([$\\, $u, U1, U2, U3, U4, $\\, $u, L1, L2, L3, L4 | Rest], Acc) ->
    % http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B10000_to_U.2B10FFFF
    Upper_offset = list_to_integer([U1, U2, U3, U4], 16),

    case (Upper_offset < 16#d800) or (Upper_offset > 16#dbff) of
        true ->
            % This is actually two sequential characters, not a surrogate pair.
            % Push the second character back on to the input and reprocess.
            string1([$\\, $u, L1, L2, L3, L4 | Rest], [Upper_offset | Acc]);
        false ->
            Lower_offset = list_to_integer([L1, L2, L3, L4], 16),
            Upper = Upper_offset - 16#d800,
            Lower = Lower_offset - 16#dc00,
            <<Codepoint_offset:20/integer>> = <<Upper:10/integer, Lower:10/integer>>,
            Codepoint = Codepoint_offset + 16#010000,
            string1(Rest, [Codepoint | Acc])
    end;
% Unicode.
string1([$\\, $u, C1, C2, C3, C4 | Rest], Acc) ->
    string1(Rest, [list_to_integer([C1, C2, C3, C4], 16) | Acc]);
% Quote termination.
string1([$\" | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
string1([Char | Rest], Acc) ->
    string1(Rest, [Char | Acc]).


% Returns an integer or float along with the rest of the input.
%
% number(Input) -> {float(), Rest_of_input} | {integer(), Rest_of_input}.
number(Input, _Options) -> number1(Input, []).

number1([], _Acc) ->
    error(unterminated_number);
number1([Char | Rest], Acc) when (Char >= 16#30 andalso Char =< 16#39);% 0 - 9
                                  Char == 16#2b;% +
                                  Char == 16#2d;% -
                                  Char == 16#2e;% .
                                  Char == 16#45;% E
                                  Char == 16#65 ->% e
    number1(Rest, [Char | Acc]);
number1(Rest, Acc) ->
    {convert_number(lists:reverse(Acc)), Rest}.

convert_number(String) ->
    case string:to_float(String) of
        {error, no_float} ->
            try list_to_integer(String) of
                Any -> Any
            catch
                error:badarg ->
                    {Number, _Rest} = string:to_float(re:replace(string:to_lower(String), "e", ".0e", [{return, list}])),
                    Integer = trunc(Number),
                    case Integer == Number of
                        true -> Integer;
                        false -> Number
                    end
            end;
        {Number, _Rest} -> Number
    end.




-ifdef(TEST).

value_test() ->
    {true, "]"} = value("true]", []),
    {false, "]"} = value("false]", []),
    {null, "]"} = value("null]", []),
    {"foobar", "]"} = value("\"foobar\"]", []),
    {0.1234, "]"} = value("0.1234]", []),
    {[true], ""} = value("[true]", []),
    {[true], ""} = value("[  true  ]", []),
    {[true, false, "Foo bar baz, buzz."], ""} = value("[true, false, \"Foo bar baz, buzz.\"]", []),
    {#{"foo" := "bar"}, ""} = value("{\"foo\": \"bar\"}", []),
    {#{"foo" := "bar", "baz" := ["b", "u", "zz"]}, ""} = value(
        " {  \"foo\" : \"bar\",\n   \"baz\": [\n\t  \"b\" , \"u\", \"zz\"\n]}", []),
    {#{"total_rows" := 0, "offset" := 0, "rows" := []}, "\n"} = value(
        "{\"total_rows\":0,\"offset\":0,\"rows\":[\r\n\r\n]}\n", []),
    ok.


string_test() ->
    ok = try string("foobar", []) of
        _ -> will_not_happen
    catch
        error:unterminated_string -> ok
    end,
    {"foobar", []} = string("foobar\"", []),
    % Literal unicode.
    {"🚀", []} = string("🚀\"", []),
    % Unicode in the basic multilingual plane.
    {"￦", []} = string("\\uFFE6\"", []),
    {"￦￦", []} = string("\\uFFE6\\uFFE6\"", []),
    % UTF-16 surrogate pairs.
    {"𝄞", []} = string("\\ud834\\udd1e\"", []),
    {"￦𝄞", []} = string("\\uFFE6\\ud834\\udd1e\"", []),
    ok.


number_test() ->
    ok = try number("1234", []) of
        _ -> will_not_happen
    catch
        error:unterminated_number -> ok
    end,
    {0.1234, ","} = number("0.1234,", []),
    {1.234, "}"} = number("0.1234e1}", []),
    {1.234, "\n"} = number("0.1234E1\n", []),
    {1.234, "]"} = number("0.1234e+1]", []),
    {0.01234, "]"} = number("0.1234e-1]", []),
    {0, "]"} = number("0]", []),
    {-1, "]"} = number("-1]", []),
    {1, "]"} = number("1]", []),
    {1234, "]"} = number("1234]", []),
    {12340, "]"} = number("1234e1]", []),
    {12340, "]"} = number("1234E1]", []),
    {12340, "]"} = number("1234e+1]", []),
    {123.4, "]"} = number("1234e-1]", []),
    {1234, "]"} = number("12340e-1]", []).
-endif.
