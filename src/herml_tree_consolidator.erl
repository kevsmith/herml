-module(herml_tree_consolidator).

-author("kevin@hypotheticalabs.com").

-include("tree.hrl").

-define(LIST_START, "[").
-define(LIST_END, "]").

-export([consolidate/1]).

consolidate(Tree) ->
  consolidate_nodes(Tree, []).

%% Internal functions
consolidate_nodes(Node, _Acc) when is_tuple(Node) ->
  Node#node{children=consolidate(Node#node.children)};
consolidate_nodes([H|T], Acc) ->
  H1 = H#node{children=consolidate(H#node.children)},
  case needs_consolidation(H1) of
    true ->
      case length(T) of
        0 ->
          {Nodes, NewChildren} = find_terminator(H1#node.children),
          if
            length(Nodes) == 0 ->
              throw({bad_list_terminator, H1#node.line});
            true ->
              H2 = H1#node{children=NewChildren},
              io:format("NewChildren: ~p~n", [NewChildren]),
              H3 = combine_nodes(H2, Nodes),
              consolidate_nodes(T, [H3|Acc])
            end;
        _ ->
          {Nodes, NewTail} = find_terminator(T),
          if
            length(Nodes) == 0 ->
              throw({bad_list_terminator, H1#node.line});
            true ->
              consolidate_nodes(NewTail, [combine_nodes(H1, Nodes)|Acc])
          end
      end;
    false ->
      consolidate_nodes(T, [H1|Acc])
  end;
consolidate_nodes([], Acc) ->
  lists:reverse(Acc).

combine_nodes(Start, [H|T]) ->
  NewStart = combine_nodes(Start, H),
  combine_nodes(NewStart, T);
combine_nodes(Start, []) ->
  Start;
combine_nodes(F, S) when is_tuple(F), is_tuple(S) ->
  S1 = consolidate(S),
  F1 = F#node{children=lists:flatten([F#node.children, S1#node.children])},
  F1#node{data=F1#node.data ++ S1#node.data}.

needs_consolidation(Node) ->
  case string:str(Node#node.data, ?LIST_START) of
    0 ->
      false;
    _ ->
      case string:str(Node#node.data, ?LIST_END) of
        0 ->
          true;
        _ ->
          false
      end
  end.

is_terminator(Node) ->
  TextLength = length(Node#node.data),
  case string:str(Node#node.data, ?LIST_END) of
    0 ->
      false;
    TextLength ->
      true;
    _ ->
      throw({bad_list_terminator, Node#node.line})
  end.

find_terminator(Nodes) ->
  find_terminator(Nodes, []).

find_terminator([H|T], Acc) ->
  case is_terminator(H) of
    false ->
      find_terminator(T, [H|Acc]);
    true ->
      {lists:reverse([H|Acc]), T}
  end;
find_terminator([], Acc) ->
  {lists:reverse(Acc), []}.
