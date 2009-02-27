-module(herml).
-export([render/2, html_attrs/0]).
-export([html_attrs/1, h/1, html_escape/1]).
-export([e/1, escape_once/1]).

render(TemplateName, Env) ->
  ManagerName = proplists:get_value("__herml_manager__", Env),
  Offset = proplists:get_value("__herml_depth__", Env),
  case herml_manager:execute_template(ManagerName, TemplateName, Env, Offset) of
    {ok, Content} ->
      {string:strip(lists:flatten(Content), right, $\n), Env};
    {error, _} ->
      {"ERROR: Unable to render " ++ TemplateName, Env}
  end.

html_attrs()->
  [{"xmlns","http://www.w3.org/1999/xhtml"}, {"xml:lang", "en-US"}, {"lang", "en-US"}].
html_attrs(Lang) ->
  [{"xmlns","http://www.w3.org/1999/xhtml"}, {"xml:lang", Lang}, {"lang", Lang}].

h(Text) ->
  html_escape(Text).

html_escape("") -> "";
html_escape(Text) ->
  WithoutAmp  =  re:replace(Text,       "&",  "\\&amp;",  [global, {return, list}]),
  WithoutLt   =  re:replace(WithoutAmp, "<",  "\\&lt;",   [global, {return, list}]),
  WithoutGt   =  re:replace(WithoutLt,  ">",  "\\&gt;",   [global, {return, list}]),
  re:replace(WithoutGt,  "\"", "\\&quot;", [global, {return, list}]).


e(Text) ->
  escape_once(Text).

escape_once("") -> "";
escape_once(Text) ->
  WithoutAmp  =  re:replace(Text, "&(?!([a-zA-Z]+|(#\\d+));)", "\\&amp;", [global, {return, list}]),
  WithoutLt   =  re:replace(WithoutAmp, "<",  "\\&lt;",   [global, {return, list}]),
  WithoutGt   =  re:replace(WithoutLt,  ">",  "\\&gt;",   [global, {return, list}]),
  re:replace(WithoutGt,  "\"", "\\&quot;", [global, {return, list}]).
