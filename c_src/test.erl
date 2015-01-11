-module(test).
-export([findkeywords/2]).

findkeywords(Word, Arr) ->
    lists:filter(fun(Desc) -> string:str(Desc, Word) > 0 end, Arr).
