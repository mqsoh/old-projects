-module(json_parser).
-export([
    value/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


value([$ | Rest]) -> value(Rest);
value([${ | Rest]) -> object(Rest);
value([$[ | Rest]) -> array(Rest);
value([$" | Rest]) -> string(Rest);
value([$t, $r, $u, $e | Rest]) -> {true, Rest};
value([$f, $a, $l, $s, $e | Rest]) -> {false, Rest};
value([$n, $u, $l, $l | Rest]) -> {null, Rest};
value(Rest) -> number(Rest).


object(Input) -> object(Input, []).

% Termination.
object([$} | Rest], Object_list) ->
    {maps:from_list(Object_list), Rest};
% Skip.
object([Char | Rest], Object_list) when
    Char == $,;
    Char == $ ;
    Char == $\n;
    Char == $\t ->
    object(Rest, Object_list);
% Add property.
object([$" | Rest], Object_list) ->
    {Key, Rest1} = string(Rest),
    Rest2 = lists:foldl(
        fun (Unwanted, String) ->
            string:strip(String, left, Unwanted)
        end,
        Rest1,
        [$ , $:, $ ]),
    {Value, Rest3} = value(Rest2),
    object(Rest3, [{Key, Value} | Object_list]).


array(Input) -> array(Input, []).

% Termination.
array([$] | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
% Skip.
array([Char | Rest], Acc) when
    Char == $,;
    Char == $ ;
    Char == $\n;
    Char == $\t ->
    array(Rest, Acc);
% Append.
array(Rest, Acc) ->
    {Value, Rest1} = value(Rest),
    array(Rest1, [Value | Acc]).


% Returns the input up until the next literal double quote, along with the
% rest of the input.
%
% string(Input) -> {Interpreted_string, Rest_of_input}.
%
% Input = string()
% Interpreted_string = string()
% Rest_of_input = string()
string(Input) ->
    string(Input, []).

string([], _Acc) ->
    error(unterminated_string);
% Escaped characters.
string([$\\, $" | Rest], Acc) -> string(Rest, ["\"", Acc]);
string([$\\, $\ | Rest], Acc) -> string(Rest, ["\\", Acc]);
string([$\\, $/ | Rest], Acc) -> string(Rest, ["/" | Acc]);
string([$\\, $b | Rest], Acc) -> string(Rest, ["\b" | Acc]);
string([$\\, $f | Rest], Acc) -> string(Rest, ["\f" | Acc]);
string([$\\, $n | Rest], Acc) -> string(Rest, ["\n" | Acc]);
string([$\\, $r | Rest], Acc) -> string(Rest, ["\r" | Acc]);
string([$\\, $t | Rest], Acc) -> string(Rest, ["\t" | Acc]);
% Unicode; UTF-16 surrogate pair.
string([$\\, $u, U1, U2, U3, U4, $\\, $u, L1, L2, L3, L4 | Rest], Acc) ->
    % http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B10000_to_U.2B10FFFF
    Upper_offset = list_to_integer([U1, U2, U3, U4], 16),

    case (Upper_offset < 16#d800) or (Upper_offset > 16#dbff) of
        true ->
            % This is actually two sequential characters, not a surrogate pair.
            % Push the second character back on to the input and reprocess.
            string([$\\, $u, L1, L2, L3, L4 | Rest], [Upper_offset | Acc]);
        false ->
            Lower_offset = list_to_integer([L1, L2, L3, L4], 16),
            Upper = Upper_offset - 16#d800,
            Lower = Lower_offset - 16#dc00,
            <<Codepoint_offset:20/integer>> = <<Upper:10/integer, Lower:10/integer>>,
            Codepoint = Codepoint_offset + 16#010000,
            string(Rest, [Codepoint | Acc])
    end;
% Unicode.
string([$\\, $u, C1, C2, C3, C4 | Rest], Acc) ->
    string(Rest, [list_to_integer([C1, C2, C3, C4], 16) | Acc]);
% Quote termination.
string([$\" | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
string([Char | Rest], Acc) ->
    string(Rest, [Char | Acc]).


% Returns an integer or float along with the rest of the input.
%
% number(Input) -> {float(), Rest_of_input} | {integer(), Rest_of_input}.
number(Input) -> number(Input, []).

number([], _Acc) ->
    error(unterminated_number);
number([Char | Rest], Acc) when (Char >= 16#30 andalso Char =< 16#39);% 0 - 9
                                Char == 16#2b;% +
                                Char == 16#2d;% -
                                Char == 16#2e;% .
                                Char == 16#45;% E
                                Char == 16#65 ->% e
    number(Rest, [Char | Acc]);
number(Rest, Acc) ->
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
    {true, "]"} = value("true]"),
    {false, "]"} = value("false]"),
    {null, "]"} = value("null]"),
    {"foobar", "]"} = value("\"foobar\"]"),
    {0.1234, "]"} = value("0.1234]"),
    {[true], ""} = value("[true]"),
    {[true], ""} = value("[  true  ]"),
    {[true, false, "Foo bar baz, buzz."], ""} = value("[true, false, \"Foo bar baz, buzz.\"]"),
    {#{"foo" := "bar"}, ""} = value("{\"foo\": \"bar\"}"),
    {#{"foo" := "bar", "baz" := ["b", "u", "zz"]}, ""} = value(
        " {  \"foo\" : \"bar\",\n   \"baz\": [\n\t  \"b\" , \"u\", \"zz\"\n]}").


string_test() ->
    ok = try string("foobar") of
        _ -> will_not_happen
    catch
        error:unterminated_string -> ok
    end,
    {"foobar", []} = string("foobar\""),
    % Literal unicode.
    {"🚀", []} = string("🚀\""),
    % Unicode in the basic multilingual plane.
    {"￦", []} = string("\\uFFE6\""),
    {"￦￦", []} = string("\\uFFE6\\uFFE6\""),
    % UTF-16 surrogate pairs.
    {"𝄞", []} = string("\\ud834\\udd1e\""),
    {"￦𝄞", []} = string("\\uFFE6\\ud834\\udd1e\""),
    ok.


number_test() ->
    ok = try number("1234") of
        _ -> will_not_happen
    catch
        error:unterminated_number -> ok
    end,
    {0.1234, ","} = number("0.1234,"),
    {1.234, "}"} = number("0.1234e1}"),
    {1.234, "\n"} = number("0.1234E1\n"),
    {1.234, "]"} = number("0.1234e+1]"),
    {0.01234, "]"} = number("0.1234e-1]"),
    {0, "]"} = number("0]"),
    {-1, "]"} = number("-1]"),
    {1, "]"} = number("1]"),
    {1234, "]"} = number("1234]"),
    {12340, "]"} = number("1234e1]"),
    {12340, "]"} = number("1234E1]"),
    {12340, "]"} = number("1234e+1]"),
    {123.4, "]"} = number("1234e-1]"),
    {1234, "]"} = number("12340e-1]").
-endif.
