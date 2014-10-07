%% This file was generated from json_parser.erl.nw.
-module(json_parser).
-export([
    value/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

number(Input, _Options) ->
    number1(Input, []).
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
    Number_string = lists:reverse(Acc),
    % Try making it a float.
    Number = case string:to_float(Number_string) of
        {error, no_float} ->
            % Failed float; try an integer.
            try list_to_integer(Number_string) of
                Any -> Any
            catch
                error:badarg ->
                    % Oops. Failed integer, too. It might be a converstion
                    % problem between JSON and Erlang's way of dealing with
                    % exponents. This will fix it, but...
                    {Float, _Rest} = string:to_float(re:replace(string:to_lower(Number_string), "e", ".0e", [{return, list}])),
                    Integer = trunc(Float),
                    % ...now we don't know if it's an integer or float.
                    case Integer == Float of
                        true -> Integer;
                        false -> Float
                    end
            end;
        {Float, _Rest} -> Float
    end,
    {Number, Rest}.
string(Input, _Options) ->
    string1(Input, []).
string1([], _Acc) ->
    error(unterminated_string);
string1([$\\, $" | Rest], Acc) -> string1(Rest, [$" | Acc]);
string1([$\\, $\\ | Rest], Acc) -> string1(Rest, [$\\ | Acc]);
string1([$\\, $/ | Rest], Acc) -> string1(Rest, [$/ | Acc]);
string1([$\\, $b | Rest], Acc) -> string1(Rest, [$\b | Acc]);
string1([$\\, $f | Rest], Acc) -> string1(Rest, [$\f | Acc]);
string1([$\\, $n | Rest], Acc) -> string1(Rest, [$\n | Acc]);
string1([$\\, $r | Rest], Acc) -> string1(Rest, [$\r | Acc]);
string1([$\\, $t | Rest], Acc) -> string1(Rest, [$\t | Acc]);
string1([$\\, $u, U1, U2, U3, U4, $\\, $u, L1, L2, L3, L4 | Rest], Acc) ->
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
string1([$\\, $u, C1, C2, C3, C4 | Rest], Acc) ->
    string1(Rest, [list_to_integer([C1, C2, C3, C4], 16) | Acc]);
string1([$\" | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
string1([Char | Rest], Acc) ->
    string1(Rest, [Char | Acc]).
value([$t, $r, $u, $e | Rest], _Options) ->
    {true, Rest};
value([$f, $a, $l, $s, $e | Rest], _Options) ->
    {false, Rest};
value([$n, $u, $l, $l | Rest], _Options) ->
    {null, Rest};
value([$" | Rest], Options) ->
    string(Rest, Options);
value([$- | Rest], Options) ->
    number([$- | Rest], Options);
value([Char | Rest], Options) when Char >= 16#30 andalso Char =< 16#39 -> % A digit 0 - 9.
    number([Char | Rest], Options);
value([Char | Rest], Options) when
    Char == $ ;
    Char == $\r;
    Char == $\n;
    Char == $\t ->
    value(Rest, Options);
value([$[ | Rest], Options) ->
    array(Rest, Options);
value([${ | Rest], Options) ->
    object(Rest, Options);
value(_Input, _Options) ->
    error(unknown_value).
array(Input, Options) ->
    array(Input, [], Options).

array([$] | Rest], Acc, _Options) ->
    {lists:reverse(Acc), Rest};
array([Char | Rest], Acc, Options) when
    Char == $,;
    Char == $ ;
    Char == $\r;
    Char == $\n;
    Char == $\t ->
    array(Rest, Acc, Options);

array(Rest, Acc, Options) ->
    {Value, Rest1} = value(Rest, Options),
    array(Rest1, [Value | Acc], Options).
object(Input, Options) ->
    object(Input, [], Options).

object([$} | Rest], Object_list, _Options) ->
    {maps:from_list(Object_list), Rest};
object([Char | Rest], Object_list, Options) when
    Char == $,;
    Char == $ ;
    Char == $\r;
    Char == $\n;
    Char == $\t ->
    object(Rest, Object_list, Options);
object([$" | Rest], Object_list, Options) ->
    {Key, Rest1} = begin
        {String_key, Rest1} = string(Rest, Options),
        case lists:member(atom_keys, Options) of
            true -> {list_to_atom(String_key), Rest1};
            _ -> {String_key, Rest1}
        end
    end,
    Rest2 = re:replace(Rest1, "^[:\\s]+", "", [global, {return, list}]),
    {Value, Rest3} = value(Rest2, Options),
    object(Rest3, [{Key, Value} | Object_list], Options).

-ifdef(TEST).
unterminated_number_test() ->
    ok = try number("1234", []) of
        _ -> will_not_happen
    catch
        error:unterminated_number -> ok
    end.
various_numbers_test() ->
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
unterminated_string_test() ->
    ok = try string("foobar", []) of
        _ -> will_not_happen
    catch
        error:unterminated_string -> ok
    end.
simple_strings_test() ->
    {"foobar", []} = string("foobar\"", []),
    {"\\", []} = string("\\\\\"", []),
    {"\".\\./.\b.\f.\n.\r.\t.", []} = string("\\\".\\.\\/.\\b.\\f.\\n.\\r.\\t.\"", []).
literal_unicode_string_test() ->
    {"😂", []} = string("😂\"", []).
multilingual_plane_string_test() ->
    {"￦", []} = string("\\uFFE6\"", []),
    {"￦￦", []} = string("\\uFFE6\\uFFE6\"", []).
utf_16_surrogate_pairs_test() ->
    {"𝄞", []} = string("\\ud834\\udd1e\"", []),
    {"￦𝄞￦", []} = string("\\uFFE6\\ud834\\udd1e\\uffe6\"", []).
simple_value_test() ->
    {true, "]"} = value("true]", []),
    {false, "]"} = value("false]", []),
    {null, "]"} = value("null]", []).
string_value_test() ->
    {"foobar", "]"} = value("\"foobar\"]", []).
number_value_test() ->
    {0.1234, "]"} = value("0.1234]", []).
ignore_white_space_value_test() ->
    {true, "]"} = value("   \n \t  \r\n  true]", []).
array_test() ->
    {[true], ""} = array("true]", []),
    {[true, false, null], ""} = array("true, false, null]", []),
    {[true, false, null], ""} = array("\r\n\ttrue  ,\n    false\n,\n    \tnull\n  ]", []),
    {[true, false, "Foo bar baz, buzz."], ""} = array("true, false, \"Foo bar baz, buzz.\"]", []).
object_test() ->
    {#{"foo" := "bar"}, ""} = object("\"foo\": \"bar\"}", []),
    {#{"foo" := "bar", "baz" := ["b", "u", "zz"]}, ""} = object(
        "  \"foo\" : \"bar\",\n   \"baz\": \n[\n\t  \"b\" , \"u\", \"zz\"\n]}", []),
    {#{"total_rows" := 0, "offset" := 0, "rows" := []}, "\n"} = object(
        "\"total_rows\":0,\"offset\":0,\"rows\":[\r\n\r\n]}\n", []).
final_test() ->
    {#{"number" := 1230000,
       "string" := "foobar",
        "array" := [true, false, null],
        "nested_object" := #{"a" := "b"}}, ""} =
    value("{\"number\": 123e4,
            \"string\": \"foobar\",
            \"array\": [true,
                        false,
                        null],
            \"nested_object\": {
                \"a\": \"b\"}}", []).
-endif.
