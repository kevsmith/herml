-module(herml).
-compile([export_all]).

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