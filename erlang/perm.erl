-module(perm).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

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

insert_231(Arr) ->
    Len = length(Arr),
    Pos = trunc(math:floor(rand:uniform() * Len)),
    {Left, Right} = lists:split(Pos, Arr),
    Left ++ [2, 3, 1] ++ Right.

list_no_dupls_inserted_231(T) ->
    ?LET(L, (list(T)), (insert_231(remove_123(L)))).

remove_123([]) -> [];
remove_123([A | T]) ->
    case A of
        1 -> remove_123(T);
        2 -> remove_123(T);
        3 -> remove_123(T);
        _Else -> [A | remove_123(T)]
    end.

list_no_dupls(T) ->
    ?LET(L, (list(T)), (remove_duplicates(L))).

%% remove duplicates taken from slides
remove_duplicates([]) -> [];
remove_duplicates([A | T]) ->
    case lists:member(A, T) of
        true -> remove_duplicates(T);
        false -> [A | remove_duplicates(T)]
    end.

%% Checks if a sequence contains a 231 pattern
contains_231([]) -> false;
contains_231(Arr) ->
    Max = lists:max(Arr),
    {A, B} = lists:splitwith(fun (A) -> A /= Max end, Arr),
    IsCont = length(A) /= 0 andalso
                 length(B) /= 0 andalso
                     lists:max(A) < Max andalso lists:min(B) < lists:max(A),
    if IsCont -> true;
       true ->
           contains_231(lists:filter(fun (El) -> El /= Max end,
                                     Arr))
    end.

prop_same_input_same_output() ->
    ?FORALL(L, (orderedlist(pos_integer())), (perm(L, L))).

same_input_same_output_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(prop_same_input_same_output(),
                                    [{to_file, user}]))).

prop_231_false() ->
    ?FORALL(L, (list_no_dupls_inserted_231(pos_integer())),
            (false == perm(lists:sort(L), L))).

false_231_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(prop_231_false(),
                                    [{to_file, user}]))).

prop_231_avoiding_true() ->
    ?FORALL(L, (list_no_dupls(pos_integer())),
            (?IMPLIES((contains_231(L) == false),
                      (true == perm(lists:sort(L), L))))).
avoiding_231_true_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(prop_231_avoiding_true(),
                                    [{to_file, user}]))).

perm_basic_231_pattern_should_return_false_test() ->
    false = perm([1, 2, 3, 4, 5, 6], [1, 2, 4, 5, 3, 6]),
    false = perm([1, 2, 3, 4, 5, 6], [2, 3, 1, 4, 5, 6]),
    false = perm([1, 2, 3, 4, 5, 6], [2, 5, 6, 3, 4, 1]),
    false = perm([1, 2, 3, 4, 5, 6], [5, 1, 2, 4, 6, 3]),
    ok.

perm_basic_test() ->
    true = perm([1, 2, 3], [3, 1, 2]),
    false = perm([1, 2, 3], [2, 3, 1]),
    ok.
