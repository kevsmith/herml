-module(herml_htmlizer).

-export([render/1]).

render(Template) ->
  render(Template, []).

%% Internal functions
render([{Depth, {tag_decl, Attrs}, []}|T], Accum) ->
  render(T, [render_tag(Depth, Attrs, "/>")|Accum]);
render([{Depth, {tag_decl, Attrs}, Children}|T], Accum) ->
  B1 = render_tag(Depth, Attrs, ">"),
  B2 = B1 ++ render(Children, []),
  render(T, [B2 ++ render_end_tag(Depth, Attrs)|Accum]);
render([{_, Text, []}|T], Accum) ->
  render(T, [Text ++ "\n"|Accum]);
render([{_, Text, Children}|T], Accum) ->
  render(T, [Text ++ render(Children)|Accum]);
render([], Accum) ->
  lists:reverse(Accum).

render_tag(Depth, Attrs, Terminator) ->
  create_whitespace(Depth) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs) ++
    Terminator ++ "\n".

render_end_tag(Depth, Attrs) ->
  create_whitespace(Depth) ++ "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_attrs(Attrs) ->
  lists:foldl(fun({Name, Value}, Accum) ->
                  case Name of
                    tag_name ->
                      Accum;
                    _ ->
                      Accum ++ " " ++ atom_to_list(Name) ++ "=\"" ++ Value ++ "\""
                  end end, "", Attrs).

create_whitespace(Depth) ->
  create_whitespace(Depth, []).

create_whitespace(0, Accum) ->
  lists:flatten(Accum);
create_whitespace(Depth, Accum) ->
  create_whitespace(Depth - 1, [" "|Accum]).
