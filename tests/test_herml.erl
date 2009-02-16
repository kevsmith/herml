-module(test_herml).

-author("kevin@hypotheticalabs.com").
-author("seancribbs@gmail.com").

-include_lib("eunit/include/eunit.hrl").

html_attrs_test_()->
  [
    ?_assertEqual(herml:html_attrs(), [{"xmlns","http://www.w3.org/1999/xhtml"}, {"xml:lang", "en-US"}, {"lang", "en-US"}]),
    ?_assertEqual(herml:html_attrs("en-GB"), [{"xmlns","http://www.w3.org/1999/xhtml"}, {"xml:lang", "en-GB"}, {"lang", "en-GB"}])
  ].

html_escape_test_() ->
  [
    ?_assertEqual("&lt;r:url foo=&quot;bar&quot; /&gt; &amp; foo",herml:html_escape("<r:url foo=\"bar\" /> & foo")),
    ?_assertEqual("&lt;r:url foo=&quot;bar&quot; /&gt; &amp; foo",herml:h("<r:url foo=\"bar\" /> & foo"))
  ].

escape_once_test_() ->
  [
    ?_assertEqual("&#8460;", herml:escape_once("&#8460;")),
    ?_assertEqual("&amp;&lt;&gt;&quot;", herml:escape_once("&<>\"")),
    ?_assertEqual("&amp;&lt;&gt;&quot;", herml:escape_once("&amp;&lt;&gt;&quot;")),
    ?_assertEqual("&#8460;", herml:e("&#8460;")),
    ?_assertEqual("&amp;&lt;&gt;&quot;", herml:e("&<>\"")),
    ?_assertEqual("&amp;&lt;&gt;&quot;", herml:e("&amp;&lt;&gt;&quot;"))
  ].