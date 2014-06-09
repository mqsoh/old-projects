-module(json).
-export([
    to_term/1,
    from_term/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


to_term(String) ->
    {Term, [] = _Rest_of_string} = json_parser:value(String),
    Term.


from_term(Term) ->
    json_emitter:term(Term).


-ifdef(TEST).

equality_test() ->
    Input = #{name => "Mason Staugler",
              age => 35,
              employed => true,
              favorite_foods => [
                  "pizza",
                  "chicken vindaloo",
                  "seltzer"]},

    Expected_output = #{"name" => "Mason Staugler",
               "age" => 35,
               "employed" => true,
               "favorite_foods" => [
                    "pizza",
                    "chicken vindaloo",
                    "seltzer"]},

    Json_document = from_term(Input),
    Converted_term = to_term(Json_document),

    Expected_output = Converted_term.

-endif.
