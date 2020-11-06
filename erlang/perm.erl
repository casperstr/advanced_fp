%% File: hello.erl
-module(perm).

-export([contains_231/1,
         perm/2,
         prop_same_input_pass/0]).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

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

prop_same_input_pass() ->
    ?FORALL(L, (orderedlist(pos_integer())), (perm(L, L))).

list_no_dupls_inserted_231(T) ->
    ?LET(L, (list(T)), (remove_duplicates(insert_231(L)))).

list_no_dupls(T) ->
    ?LET(L, (list(T)), (remove_duplicates(L))).

%% better versions of remove_duplicates/1 exist

remove_duplicates([]) -> [];
remove_duplicates([A | T]) ->
    case lists:member(A, T) of
        true -> remove_duplicates(T);
        false -> [A | remove_duplicates(T)]
    end.

insert_231(Arr) ->
    Len = length(Arr),
    Pos = trunc(math:floor(rand:uniform() * Len)),
    {Left, Right} = lists:split(Pos, Arr),
    Left ++ [2, 3, 1] ++ Right.

contains_231([]) -> false;
contains_231([_]) -> false;
contains_231([_, _]) -> false;
contains_231([X, Y, Z | Tail]) ->
    IsPrefix = Z < X andalso X < Y,
    if IsPrefix -> true;
       true -> contains_231([X, Y, Z | Tail])
    end.

prop_231_false() ->
    ?FORALL(L, (list_no_dupls_inserted_231(pos_integer())),
            (?IMPLIES((contains_231(L)),
                      (false == perm(lists:sort(L), L))))).

prop_231_avoiding_true() ->
    ?FORALL(L, (list_no_dupls(pos_integer())),
            (?IMPLIES((contains_231(L) == false),
                      (true == perm(lists:sort(L), L))))).

perm_basic_test() ->
    true = perm([1, 2, 3], [3, 1, 2]),
    false = perm([1, 2, 3], [2, 3, 1]),
    ok.
