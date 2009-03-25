-module(herml_htmlizer).

-export([render/1, render/2, render/3]).

-define(RESERVED_TAG_ATTRS, [tag_name, singleton]).

-define(DOCTYPE_TRANSITIONAL, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n").
-define(DOCTYPE_STRICT, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n").
-define(DOCTYPE_HTML11, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n").
-define(DOCTYPE_XML_START, "<?xml version='1.0' encoding='").
-define(DOCTYPE_XML_END, "' ?>\n").
-define(DOCTYPE_XML, ?DOCTYPE_XML_START ++ "utf-8" ++ ?DOCTYPE_XML_END).
-define(DOCTYPE_FRAMESET, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n").

render(Template) ->
  render(Template, 0).

render(Template, Offset) ->
  render(Template, [], Offset).

render(Template, Env, Offset) ->
  render(Template, Env, [], Offset).

%% Internal functions
render([{_, {iter, Match, {var_ref, List}}, Subtemplate}|T], Env, Accum, Offset) when is_list(Accum) ->
  Result = lists:map(fun(Item) ->
                         unindent(render(Subtemplate, iteration_env(Match, Item, Env), [], Offset)) end,
                     lookup_var(List, Env)),
  render(T, Env, [Result|Accum], Offset);

render([{Depth, {tag_decl, Attrs}, []}|T], Env, Accum, Offset) when is_list(Accum) ->
  CloseTag = case detect_terminator(Attrs) of
    ">" ->
      render_inline_end_tag(Attrs);
    _ ->
      "\n"
  end,
  render(T, Env, [render_inline_tag(Depth, Attrs, detect_terminator(Attrs), Env, Offset) ++ CloseTag|Accum], Offset);

render([{Depth, {tag_decl, Attrs}, Children}|T], Env, Accum, Offset) when is_list(Accum) ->
  B1 = render_tag(Depth, Attrs, ">", Env, Offset),
  B2 = B1 ++ render(Children, Env, Offset),
  render(T, Env, [B2 ++ render_end_tag(Depth, Attrs, Offset)|Accum], Offset);

render([{Depth, {var_ref, VarName}, []}|T], Env, Accum, Offset) when is_list(Accum) ->
  render(T, Env, [create_whitespace(Depth + Offset) ++ lookup_var(VarName, Env) ++ "\n"|Accum], Offset);

render([{_, {var_ref, VarName}, Children}|T], Env, Accum, Offset) ->
  render(T, Env, [lookup_var(VarName, Env) ++ render(Children, Env, Offset) | Accum], Offset);

render([{Depth, {fun_call, Module, Fun, Args}, Children}|T], Env, Accum, Offset) ->
  Result = create_whitespace(Depth + Offset) ++ invoke_fun(Module, Fun, Args, Env) ++ "\n",
  render(T, Env, [Result ++ render(Children, Env, Offset) | Accum], Offset);

render([{Depth, {fun_call_env, Module, Fun, Args}, Children}|T], Env, Accum, Offset) ->
  {R, NewEnv} = invoke_fun_env(Module, Fun, Args, Env, Depth + Offset),
  WS = create_whitespace(Depth + Offset),
  Result = case string:str(R, WS) of
             0 ->
               WS ++ R ++ "\n";
             _ ->
               R ++ "\n"
           end,
  render(T, Env, [Result ++ render(Children, NewEnv, Offset) | Accum], Offset);

render([{_, {doctype, "Transitional", _}, []}|T], Env, Accum, Offset) ->
  render(T, Env, [?DOCTYPE_TRANSITIONAL|Accum], Offset);

render([{_, {doctype, "Strict", _}, []}|T], Env, Accum, Offset) ->
  render(T, Env, [?DOCTYPE_STRICT|Accum], Offset);

render([{_, {doctype, "1.1", _}, []}|T], Env, Accum, Offset) ->
  render(T, Env, [?DOCTYPE_HTML11|Accum], Offset);

render([{_, {doctype, "XML", Encoding}, []}|T], Env, Accum, Offset) when is_list(Encoding),
                                                                         Encoding /= [] ->
  render(T, Env, lists:reverse(?DOCTYPE_XML_START ++ Encoding ++ ?DOCTYPE_XML_END) ++ Accum, Offset);

render([{_, {doctype, "XML", []}, []}|T], Env, Accum, Offset) ->
  render(T, Env, [?DOCTYPE_XML|Accum], Offset);

render([{_, {doctype, "Frameset", _}, []}|T], Env, Accum, Offset) ->
  render(T, Env, [?DOCTYPE_FRAMESET|Accum], Offset);

render([{Depth, Text, []}|T], Env, Accum, Offset) ->
  render(T, Env, [render_text(Text, Depth, Offset) ++ "\n"|Accum], Offset);

render([{Depth, Text, Children}|T], Env, Accum, Offset) ->
  render(T, Env, [render_text(Text, Depth, Offset) ++ render(Children, Env, Offset)|Accum], Offset);

render([], _Env, Accum, _Offset) ->
  lists:reverse(Accum).

render_text({text, _, Text}, Depth, Offset) ->
  create_whitespace(Depth + Offset) ++ string:strip(Text).

render_tag(Depth, Attrs, Terminator, Env, Offset) ->
  create_whitespace(Depth + Offset) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs, Env, Depth + Offset) ++
    Terminator ++ "\n".

render_inline_tag(Depth, Attrs, Terminator, Env, Offset) ->
  create_whitespace(Depth + Offset) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs, Env, Depth + Offset) ++
    Terminator.

render_end_tag(Depth, Attrs, Offset) ->
  create_whitespace(Depth + Offset) ++ "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_inline_end_tag(Attrs) ->
  "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_attrs(Attrs, Env, TotalDepth) ->
  lists:foldl(fun(Attr, Accum) ->
                render_attr(Attr, Env, Accum, TotalDepth) end, "",
              lists:sort(consolidate_classes(Attrs))).

create_whitespace(Depth) ->
  create_whitespace(Depth, []).

create_whitespace(0, Accum) ->
  lists:flatten(Accum);
create_whitespace(Depth, Accum) ->
  create_whitespace(Depth - 1, ["  "|Accum]).

render_attr({fun_call, Module, Fun, Args}, Env, Accum, TotalDepth) ->
  render_attrs(invoke_fun(Module, Fun, Args, Env), Env, TotalDepth) ++ Accum;

render_attr({fun_call_env, Module, Fun, Args}, Env, Accum, TotalDepth) ->
  {R, NewEnv} = invoke_fun_env(Module, Fun, Args, Env, TotalDepth),
  render_attrs(R, NewEnv, TotalDepth) ++ Accum;

render_attr({Name, {var_ref, VarName}}, Env, Accum, _TotalDepth) ->
  Accum ++ " " ++ render_attr_key(Name, Env) ++ "=\"" ++ lookup_var(VarName, Env) ++ "\"";
render_attr({Name, Value}, Env, Accum, _TotalDepth) ->
  case lists:member(Name, ?RESERVED_TAG_ATTRS) of
    true ->
      Accum;
    false ->
      Accum ++ " " ++ render_attr_key(Name, Env) ++ "=\"" ++ Value ++ "\""
  end.

render_attr_key({var_ref, Value}, Env) ->
  lookup_var(Value, Env);
render_attr_key(Name, _Env) ->
  atom_to_list(Name).

invoke_fun(Module, Fun, Args, Env) ->
  FinalArgs = resolve_args(Args, Env),
  apply(Module, Fun, FinalArgs).

invoke_fun_env(Module, Fun, Args, Env, TotalDepth) ->
  FinalEnv = [{"__herml_depth__", TotalDepth}|Env],
  FinalArgs = resolve_args(Args, Env) ++ [FinalEnv],
  apply(Module, Fun, FinalArgs).

resolve_args(Args, Env) ->
  resolve_args(Args, Env, []).

resolve_args([{Type, Value}|T], Env, Accum) when Type =:= string;
                                                 Type =:= number ->
  resolve_args(T, Env, [Value | Accum]);
resolve_args([{var_ref, VarName}|T], Env, Accum) ->
  resolve_args(T, Env, [lookup_var(VarName, Env)|Accum]);
resolve_args([], _Env, Accum) ->
  lists:reverse(Accum).

lookup_var(VarName, Env) ->
  format(proplists:get_value(VarName, Env, ""), Env).

format(V, Env) when is_function(V) ->
  VR = V(Env),
  format(VR, Env);
format(V, _Env) when is_integer(V) ->
  integer_to_list(V);
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

iteration_env({tuple, Matches}, Item, Env) ->
  iteration_env_list(Matches, tuple_to_list(Item), Env);
iteration_env({list, Matches}, Item, Env) ->
  iteration_env_list(Matches, Item, Env);
iteration_env(ignore, _Item, Env) ->
  Env;
iteration_env({var_ref, Name}, Item, Env) ->
  [{Name, Item}|Env];
iteration_env(_,_,Env) -> Env.

iteration_env_list([Match|Matches], [Item|Items], Env) ->
  iteration_env_list(Matches, Items, iteration_env(Match, Item, Env));
iteration_env_list(Matches, [], _Env) when is_list(Matches),
                                           length(Matches) > 0 ->
  throw(bad_match);
iteration_env_list([], [], Env) ->
  Env;
iteration_env_list([], _, _Env) ->
  throw(bad_match).

unindent(List) ->
  Flat = lists:flatten(List),
  Split = string:tokens(Flat, "\n"),
  Stripped = lists:map(fun strip_leading_indent/1, Split),
  string:join(Stripped, "\n") ++ "\n".

strip_leading_indent([32,32|T]) -> T;
strip_leading_indent(String) -> String.
