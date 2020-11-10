-module(kcolor).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([addEdge/3,
         addNode/2,
         completeConnectedGraph/1,
         createGraph/0,
         getAdjecentNodes/2,
         getNodes/1,
         graphFromNodes/1,
         isSafeColor/3,
         kcolor/2]).

isSafeColor(Neighbors, NodesWithColor, Color) ->
    NeighborsColors = lists:filtermap(fun ({L, C}) ->
                                              IsMember = lists:member(L,
                                                                      Neighbors),
                                              if IsMember == true -> {true, C};
                                                 IsMember == false -> false
                                              end
                                      end,
                                      NodesWithColor),
    lists:member(Color, NeighborsColors) == false.

kcolor(Graph, MaxColors) ->
    kcolor(Graph, [], MaxColors, 0).

kcolor(_, _, MaxColors, CurrentColor)
    when CurrentColor > MaxColors ->
    false;
kcolor([], NodesWithColor, _, _) ->
    lists:reverse(lists:map(fun ({Label, Color}) ->
                                    {Label, [Color + $a]}
                            end,
                            NodesWithColor));
kcolor([Head | Tail], NodesWithColor, MaxColors,
       CurrentColor) ->
    {Label, Neighbors} = Head,
    IsSafe = isSafeColor(Neighbors,
                         NodesWithColor,
                         CurrentColor),
    if IsSafe == true ->
           kcolor(Tail,
                  [{Label, CurrentColor}] ++ NodesWithColor,
                  MaxColors,
                  0);
       IsSafe == false ->
           kcolor([Head | Tail],
                  NodesWithColor,
                  MaxColors,
                  CurrentColor + 1)
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

adjecent_complete_prop_test() ->
    ?assertEqual(true,
                 (proper:quickcheck(adjecent_complete_prop(),
                                    [{to_file, user}]))).
