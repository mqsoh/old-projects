-module(json).
-export([
    to_term/1,
    to_term/2,
    from_term/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


to_term(String) ->
    to_term(String, []).

to_term(String, Options) ->
    % Assumes the rest of the input is okay to be ignored.
    {Term, _Rest_of_string} = json_parser:value(String, Options),
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
                  "seltzer"],
              pet => #{name => "Sasha",
                       type => "dog",
                       personality => "pig"}},

    Json_document = from_term(Input),

    % Keys to atoms.
    Input = to_term(Json_document, [atom_keys]),

    Expected_output = #{"name" => "Mason Staugler",
                        "age" => 35,
                        "employed" => true,
                        "favorite_foods" => [
                             "pizza",
                             "chicken vindaloo",
                             "seltzer"],
                        "pet" => #{"name" => "Sasha",
                                   "type" => "dog",
                                   "personality" => "pig"}},
    Converted_term = to_term(Json_document),
    Expected_output = Converted_term,

    ok.

trailing_whitespace_test() ->
    ["foobar"] = to_term("[\"foobar\"]\n").

-endif.
