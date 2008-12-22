-module(herml_htmlizer).

-export([render/1, render/2]).

-define(RESERVED_TAG_ATTRS, [tag_name, singleton]).

render(Template) ->
  render(Template, []).

render(Template, Env) ->
  render(Template, Env, []).

%% Internal functions
render([{Depth, {tag_decl, Attrs}, []}|T], Env, Accum) ->
  CloseTag = case detect_terminator(Attrs) of
    ">" ->
      render_inline_end_tag(Attrs);
    _ ->
      "\n"
  end,
  render(T, Env, [render_inline_tag(Depth, Attrs, detect_terminator(Attrs), Env) ++ CloseTag|Accum]);

render([{Depth, {tag_decl, Attrs}, Children}|T], Env, Accum) ->
  B1 = render_tag(Depth, Attrs, ">", Env),
  B2 = B1 ++ render(Children, Env),
  render(T, Env, [B2 ++ render_end_tag(Depth, Attrs)|Accum]);

render([{Depth, {var_ref, VarName}, []}|T], Env, Accum) ->
  render(T, Env, [create_whitespace(Depth) ++ lookup_var(VarName, Env) ++ "\n"|Accum]);


render([{_, {var_ref, VarName}, Children}|T], Env, Accum) ->
  render(T, Env, [lookup_var(VarName, Env) ++ render(Children, Env) |Accum]);

render([{_, {doctype, "Transitional"}, []}|T], Env, Accum) ->
  render(T, Env, ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"|Accum]);

render([{_, {doctype, "Strict"}, []}|T], Env, Accum) ->
  render(T, Env, ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"|Accum]);

render([{_, {doctype, "1.1"}, []}|T], Env, Accum) ->
  render(T, Env, ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n"|Accum]);

render([{_, {doctype, "XML"}, []}|T], Env, Accum) ->
  render(T, Env, ["<?xml version='1.0' encoding='utf-8' ?>\n"|Accum]);

render([{_, {doctype, "Frameset"}, []}|T], Env, Accum) ->
  render(T, Env, ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n"|Accum]);

render([{_, Text, []}|T], Env, Accum) ->
  render(T, Env, [render_text(Text) ++ "\n"|Accum]);

render([{_, Text, Children}|T], Env, Accum) ->
  render(T, Env, [render_text(Text) ++ render(Children, Env)|Accum]);

render([], _Env, Accum) ->
  lists:reverse(Accum).

render_text({text, _, Text}) ->
  Text.

render_tag(Depth, Attrs, Terminator, Env) ->
  create_whitespace(Depth) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs, Env) ++
    Terminator ++ "\n".

render_inline_tag(Depth, Attrs, Terminator, Env) ->
  create_whitespace(Depth) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs, Env) ++
    Terminator.

render_end_tag(Depth, Attrs) ->
  create_whitespace(Depth) ++ "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_inline_end_tag(Attrs) ->
  "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_attrs(Attrs, Env) ->
  lists:foldl(fun(Attr, Accum) ->
                render_attr(Attr, Env, Accum) end, "", 
              lists:sort(consolidate_classes(Attrs))).

create_whitespace(Depth) ->
  create_whitespace(Depth, []).

create_whitespace(0, Accum) ->
  lists:flatten(Accum);
create_whitespace(Depth, Accum) ->
  create_whitespace(Depth - 1, ["  "|Accum]).

render_attr({fun_call, Module, Fun}, Env, Accum) ->
  R1 = Module:Fun(Env),
  render_attrs(R1, Env) ++ Accum;

render_attr({Name, {var_ref, VarName}}, Env, Accum) ->
  Accum ++ " " ++ atom_to_list(Name) ++ "=\"" ++ lookup_var(VarName, Env) ++ "\"";
render_attr({Name, Value}, _Env, Accum) ->
  case lists:member(Name, ?RESERVED_TAG_ATTRS) of
    true ->
      Accum;
    false ->
      Accum ++ " " ++ atom_to_list(Name) ++ "=\"" ++ Value ++ "\""
  end.

lookup_var(VarName, Env) ->
  format(proplists:get_value(VarName, Env, ""), Env).

format(V, Env) when is_function(V) ->
  VR = V(Env),
  format(VR, Env);
format(V, _Env) when is_list(V) ->
  V;
format(V, _Env) ->
  lists:flatten(io_lib:format("~p", V)).

detect_terminator(Attrs) ->
  case proplists:get_value(singleton, Attrs, false) of
    true ->
      " />";
    false ->
      ">"
  end.

consolidate_classes(Attrs) ->
  case proplists:is_defined(class, Attrs) of
    true ->
      Classes = proplists:get_all_values(class, Attrs),
      NewValue = string:join(Classes, " "),
      [{class, NewValue}|proplists:delete(class, Attrs)];
    _ -> Attrs
  end.