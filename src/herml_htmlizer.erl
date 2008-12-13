-module(herml_htmlizer).

-export([render/1]).

render(Template) ->
  render(Template, []).

%% Internal functions
render([{{tag_decl, Attrs}, []}|T], Accum) ->
  render(T, [render_tag(Attrs, "/>")|Accum]);
render([{{tag_decl, Attrs}, Children}|T], Accum) ->
  B1 = render_tag(Attrs, ">"),
  B2 = B1 ++ render(Children, []),
  render(T, [B2 ++ render_end_tag(Attrs)|Accum]);
render([{Text, []}|T], Accum) ->
  render(T, [Text ++ "\n"|Accum]);
render([{Text, Children}|T], Accum) ->
  render(T, [Text ++ render(Children)|Accum]);
render([], Accum) ->
  lists:reverse(Accum).

render_tag(Attrs, Terminator) ->
  "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs) ++
    Terminator ++ "\n".

render_end_tag(Attrs) ->
  "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_attrs(Attrs) ->
  lists:foldl(fun({Name, Value}, Accum) ->
                  case Name of
                    tag_name ->
                      Accum;
                    _ ->
                      Accum ++ " " ++ atom_to_list(Name) ++ "=\"" ++ Value
                  end end, "", Attrs).
