-module(herml_parser).

-author("kevin@hypotheticalabs.com").

-export([file/1, string/1]).

file(FileName) ->
  case herml_reader:file(FileName) of
    {error, Error} ->
      {error, Error};
    Contents ->
      parse(Contents)
  end.

string(Template) ->
  case herml_reader:string(Template) of
    {error, Error} ->
      {error, Error};
    Contents ->
      parse(Contents)
  end.

%% Internal function
parse(Contents) ->
  parse(Contents, []).

parse([{Depth, Text, []}|T], Accum) ->
  case herml_scan:string(Text) of
    {error, _} ->
      parse(T, [{Depth, Text, []}|Accum]);
    {ok, Tokens, _} ->
      case herml_parse:parse(Tokens) of
        {ok, Stmt} ->
          parse(T, [{Depth, Stmt, []}|Accum]);
        {error, _} ->
          parse(T, [{Depth, Text, []}|Accum])
      end
  end;
parse([{Depth, Text, Children}|T], Accum) ->
  C = parse(Children, []),
  case herml_scan:string(Text) of
    {error, _} ->
      parse(T, [{Depth, Text, C}|Accum]);
    {ok, Tokens, _} ->
      case herml_parse:parse(Tokens) of
        {ok, Stmt} ->
          parse(T, [{Depth, Stmt, C}|Accum]);
        {error, _} ->
          parse(T, [{Depth, Text, Children}|Accum])
      end
  end;
parse([], Accum) ->
  lists:reverse(Accum).
