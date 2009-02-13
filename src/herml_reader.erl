-module(herml_reader).

-include("herml_tree.hrl").

-export([string/1, file/1]).

-define(EOLS, [$\r, $\n]).
-define(TAG_START, [$\%, $., $#]).
-define(INDENT, "  ").


-spec(file/1 :: (Filename :: string()) -> list() | {'error', string() | atom()}).
file(Filename) when is_list(Filename) ->
  case file:read_file(Filename) of
    {ok, Contents} ->
      herml_reader:string(Contents);
    Error ->
      Error
  end.

-spec(string/1 :: (Template :: binary()) -> list() | {'error', string() | atom()}).
string(Template) when is_binary(Template) ->
  string(binary_to_list(Template));
string(Template) when is_list(Template) ->
  parse_nodes(Template).

%% Internal functions

parse_nodes(Contents) ->
  Lines = string:tokens(Contents, ?EOLS),
  P1 = parse(Lines),
  MaxDepth = find_max_depth(P1, 0),
  N = rollup(lists:reverse(P1), MaxDepth),
  clean(N, []).

clean([{node, Depth, Text, Children}|T], Accum) ->
  C = case length(Children) of
        0 ->
          [];
        _ ->
          clean(Children, [])
      end,
  clean(T, [{Depth, Text, C}|Accum]);
clean([], Accum) ->
  lists:reverse(Accum).

rollup(Tree, -1) ->
  lists:reverse(Tree);
rollup(Tree, Depth) when is_number(Depth) ->
  rollup(parent(Tree, Depth, Depth - 1), Depth - 1).

parent(Tree, ChildDepth, ParentDepth) ->
  parent(Tree, ChildDepth, ParentDepth, [], []).

parent([H|T], ChildDepth, ParentDepth, Children, Accum) ->
  case H of
    {_, ChildDepth, _, _} ->
      parent(T, ChildDepth, ParentDepth, [H|Children], Accum);
    {NodeType, ParentDepth, Text, _} ->
      parent(T, ChildDepth, ParentDepth, [], [{NodeType, ParentDepth, Text, Children}|Accum]);
    _ ->
      parent(T, ChildDepth, ParentDepth, Children, [H|Accum])
  end;

parent([], _, _, Children, []) ->
  lists:reverse(Children);

parent([], _, _, _, Accum) ->
  lists:reverse(Accum).

find_max_depth([{node, Level, _, _}|T], Max) ->
  case Level > Max of
    true ->
      find_max_depth(T, Level);
    false ->
      find_max_depth(T, Max)
  end;
find_max_depth([], Max) ->
  Max.

parse(Lines) ->
  parse(Lines, []).

parse([H|T], Accum) ->
  L = string:strip(H),
  case length(L) of
    0 ->
      parse(T, Accum);
    _ ->
      parse(T, [classify(H)|Accum])
  end;
parse([], Accum) ->
  lists:reverse(Accum).

classify(Line) ->
  Indent = count_indent(Line, 0),
  case Indent rem 2 of
    0 ->
      {node, Indent div 2, Line, []};
    _ ->
      throw({error, bad_indent, Line})
  end.

count_indent([$\s|T], Count) ->
  count_indent(T, Count + 1);
count_indent([_|_], Count) ->
  Count.