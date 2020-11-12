-module(kcolor).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([addEdge/3,
         addNode/2,
         adjecent_complete_connected_graph_color/0,
         completeConnectedGraph/1,
         createGraph/0,
         getAdjecentNodes/2,
         getNodes/1,
         graphFromNodes/1,
         kcolor/2]).

isSafe(Graph, NodeIndex, Colors, Color) ->
    {_, AdjecentLabels} = lists:nth(NodeIndex, Graph),
    maps:size(maps:filter(fun (Key, Val) ->
                                  (Val == Color) and
                                      lists:member(Key, AdjecentLabels)
                          end,
                          Colors))
        == 0.

kcolor(Graph, MaxColors) ->
    ColorMap = kcolor(Graph, 1, #{}, 0, MaxColors),
    if ColorMap == false -> false;
       true ->
           lists:map(fun ({Index, {Label, _}}) ->
                             {Label, [$a + maps:get(Index, ColorMap)]}
                     end,
                     lists:zip(lists:seq(1, length(Graph)), Graph))
    end.

kcolor(_, _, _, CurrentColor, MaxColors)
    when CurrentColor + 1 > MaxColors ->
    false;
kcolor(Graph, _, Colors, _, _)
    when map_size(Colors) == length(Graph) ->
    Colors;
kcolor(Graph, NodeIndex, Colors, CurrentColor,
       MaxColors) ->
    IsSafe = isSafe(Graph, NodeIndex, Colors, CurrentColor),

    if IsSafe == true ->
           NewColors = maps:put(NodeIndex, CurrentColor, Colors),
           NextColor = kcolor(Graph,
                              NodeIndex + 1,
                              NewColors,
                              0,
                              MaxColors),
           if NextColor == false -> false;
              true -> NextColor
           end;
       IsSafe == false ->
           kcolor(Graph,
                  NodeIndex,
                  Colors,
                  CurrentColor + 1,
                  MaxColors)
    end.

createGraph() -> [].

addNode(Graph, Label) -> Graph ++ [{Label, []}].

getAdjecentNodes([Node | Tail], LabelA) ->
    case Node of
        {Label, Neighbors} when Label == LabelA -> Neighbors;
        _Else -> getAdjecentNodes(Tail, LabelA)
    end.

addEdgeAux(Node, LabelA, LabelB) ->
    case Node of
        {Label, Neighbors} when Label == LabelA ->
            {Label, Neighbors ++ [LabelB]};
        {Label, Neighbors} when Label == LabelB ->
            {Label, Neighbors ++ [LabelA]};
        _Else -> Node
    end.

addEdge(Graph, LabelA, LabelB) ->
    lists:map(fun (Node) -> addEdgeAux(Node, LabelA, LabelB)
              end,
              Graph).

getNodes(Graph) ->
    lists:map(fun ({Label, _}) -> Label end, Graph).

%% TESTS

%% remove duplicates taken from slides
remove_duplicates([]) -> [];
remove_duplicates([A | T]) ->
    case lists:member(A, T) of
        true -> remove_duplicates(T);
        false -> [A | remove_duplicates(T)]
    end.

list_no_dupls(T) ->
    ?LET(L, (list(T)), (remove_duplicates(L))).

graphFromNodes(Arr) ->
    Graph = createGraph(),
    lists:foldr(fun (Elem, Acc) -> addNode(Acc, Elem) end,
                Graph,
                Arr).

completeConnectedGraph(Graph) ->
    Nodes = getNodes(Graph),
    completeConnectedGraph(Nodes, Graph).

completeConnectedGraph([], Graph) -> Graph;
completeConnectedGraph([A | Tail], Graph) ->
    completeConnectedGraph(Tail,
                           lists:foldr(fun (B, Acc) -> addEdge(Acc, A, B) end,
                                       Graph,
                                       Tail)).

node_lengh_prop() ->
    ?FORALL(L, (list_no_dupls(pos_integer())),
            (length(getNodes(graphFromNodes(L))) =:= length(L))).

node_lengh_prop_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(node_lengh_prop(),
                                    [{to_file, user}]))).

adjecent_complete_prop() ->
    ?FORALL(L, (list_no_dupls(pos_integer())),
            (lists:all(fun (El) ->
                               lists:sort(getAdjecentNodes(completeConnectedGraph(graphFromNodes(L)),
                                                           El))
                                   =:=
                                   lists:sort(lists:filter(fun (A) -> A /= El
                                                           end,
                                                           L))
                       end,
                       L))).

kcolor_result_labels_equal_arg_labels_prop() ->
    ?FORALL(L, (list_no_dupls(pos_integer())),
            (lists:sort(lists:map(fun ({Label, _}) -> Label end,
                                  kcolor(completeConnectedGraph(graphFromNodes(L)),
                                         length(L) + 1)))
                 =:= lists:sort(L))).

kcolor_result_labels_equal_arg_labels_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(kcolor_result_labels_equal_arg_labels_prop(),
                                    [{to_file, user}]))).

adjecent_complete_prop_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(adjecent_complete_prop(),
                                    [{to_file, user}]))).

adjecent_complete_connected_graph_color() ->
    ?FORALL(L, (list_no_dupls(pos_integer())),
            (length(kcolor(completeConnectedGraph(graphFromNodes(L)),
                           length(L) + 1))
                 == length(L))).

adjecent_complete_connected_graph_color_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(adjecent_complete_connected_graph_color(),
                                    [{to_file, user}]))).
