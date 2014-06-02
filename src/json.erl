-module(json).
-export([
    from_string/1
]).


from_string(String) ->
    json_parser:value(String).
