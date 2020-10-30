-module(kcolor).

-export([addEdge/3,
         addNode/2,
         kcolor/2,
         getNodes/1,
         isSafeColor/3]).



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

kcolor(Graph,MaxColors) -> kcolor(Graph, [],MaxColors, 0).

kcolor(_, _,MaxColors, CurrentColor) when CurrentColor > MaxColors -> false;
kcolor([], NodesWithColor,_, _) -> NodesWithColor;
kcolor([Head | Tail], NodesWithColor,MaxColors, CurrentColor) ->

    {Label, Neighbors} = Head,
    IsSafe = isSafeColor(Neighbors,
                         NodesWithColor,
                         CurrentColor),
    if IsSafe == true ->
           kcolor(Tail,
                      [{Label, CurrentColor}] ++ NodesWithColor,MaxColors,
                      0);
       IsSafe == false ->
           kcolor([Head | Tail],
                      NodesWithColor,MaxColors,
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
