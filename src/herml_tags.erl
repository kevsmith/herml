-module(herml_tags).

-author("kevin@hypotheticalabs.com").

-include("tree.hrl").

%% !
-define(FUNCALL_TAG, 33).
%% #
-define(DIV_ID_TAG, 35).
%% %
-define(GENERIC_TAG, 37).
%% .
-define(DIV_CLASS_TAG, 46).
%% @
-define(VAR_TAG, 64).

-define(ATTRS_START, "[{").
-define(ATTRS_END, "}]").

-export([analyze/1]).

-spec(analyze/1 :: (Line :: string()) -> any()).
analyze(L) ->
  Line = string:strip(L),
  convert(erlang:hd(Line), Line, L).

%% Internal functions

convert(?DIV_ID_TAG, Line, _) ->
  #herml_node{type=tag, attrs=[{tag_name, "div"}|parse_div(id, Line)]};
convert(?DIV_CLASS_TAG, Line, _) ->
  #herml_node{type=tag, attrs=[{tag_name, "div"}|parse_div(class, Line)]};
convert(?GENERIC_TAG, Line, _) ->
  {Name, Attrs} = case has_attrs(Line) of
                    true ->
                      parse_attrs(erlang:tl(Line));
                    false ->
                      {erlang:tl(Line), []}
                  end,
  #herml_node{type=tag, attrs=[{tag_name, Name}|Attrs]};
convert(_, _, L) ->
  #herml_node{type=text, attrs=[{body, L}]}.

parse_div(MainAttr, Line) ->
  {Name, Attrs} = case has_attrs(Line) of
                    true ->
                      parse_attrs(erlang:tl(Line));
                    false ->
                      {erlang:tl(Line), []}
                  end,
  [{MainAttr, Name}|Attrs].

has_attrs(Text) when length(Text) > 1 ->
  string:str(Text, ?ATTRS_START) > 0 andalso string:str(Text, ?ATTRS_END) > 0;
has_attrs(_Text) ->
  false.

parse_attrs(Text) ->
  Start = string:str(Text, ?ATTRS_START),
  End = string:str(Text, ?ATTRS_END),
  RawAttrs = string:substr(Text, Start, (End + Start)) ++ ".",
  {ok, T, _} = erl_scan:string(RawAttrs),
  {ok, Attrs} = erl_parse:parse_term(T),
  {string:strip(string:left(Text, Start - 1)), Attrs}.
