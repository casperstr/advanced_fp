-module(kcolor).

-export([addEdge/3,
         addNode/2,
         colorGraph/1,
         getNodes/1,
         isSafeColor/3]).

kcolor(graph) -> true.

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

colorGraph(Graph) -> colorGraph(Graph, [], 0).

colorGraph([], NodesWithColor, _) -> NodesWithColor;
colorGraph([Head | Tail], NodesWithColor,
           CurrentColor) ->
    {Label, Neighbors, _} = Head,
    IsSafe = isSafeColor(Neighbors,
                         NodesWithColor,
                         CurrentColor),
    if IsSafe == true ->
           colorGraph(Tail,
                      [{Label, CurrentColor}] ++ NodesWithColor,
                      0);
       IsSafe == false ->
           colorGraph([Head | Tail],
                      NodesWithColor,
                      CurrentColor + 1)
    end.

addNode(Graph, Label) ->
    Graph ++ [{Label, []}].

addEdgeAux(Node, LabelA, LabelB) ->
    case Node of
        {Label, Neighbors} when Label == LabelA ->
            {Label, Neighbors ++ [LabelB], 0};
        {Label, Neighbors} when Label == LabelB ->
            {Label, Neighbors ++ [LabelA], 0}
    end.

addEdge(Graph, LabelA, LabelB) ->
    lists:map(fun (Node) -> addEdgeAux(Node, LabelA, LabelB)
              end,
              Graph).

getNodes(Graph) ->
    lists:map(fun ({Label, _}) -> Label end, Graph).
