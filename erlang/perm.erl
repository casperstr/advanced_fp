%% File: hello.erl
-module(perm).

-export([perm/2]).

perm(Inp, Out) ->
    isStackPermutation(lists:reverse(Inp),
                       lists:reverse(Out),
                       []).

isStackPermutation([], Out, Stack) -> Out =:= Stack;
isStackPermutation([CurrentVal | NextInput],
                   [CurrentOut | NextOut], []) ->
    isStackPermutation(NextInput,
                       [CurrentOut | NextOut],
                       [CurrentVal]);
isStackPermutation([CurrentVal | NextInput],
                   [CurrentOut | NextOut], [TopOfStack | RestStack]) ->
    if CurrentOut == TopOfStack ->
           isStackPermutation([CurrentVal | NextInput],
                              NextOut,
                              RestStack);
       true ->
           isStackPermutation(NextInput,
                              [CurrentOut | NextOut],
                              [CurrentVal] ++ [TopOfStack | RestStack])
    end.
