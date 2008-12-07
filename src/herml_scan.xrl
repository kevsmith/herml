Definitions.

WS  = [\s|\t|\n|\r]
D = [0-9]
STRCHAR = [a-z|A-Z|0-9|_|-]

Rules.

{WS}+                   :       skip_token.
\%                      :       {token, {tag_start, TokenLine, TokenChars}}.
\.                      :       {token, {class_start, TokenLine, TokenChars}}.
#                       :       {token, {id_start, TokenLine, TokenChars}}.
{D}+                    :       {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{STRCHAR}+              :       {token, {name, TokenLine, TokenChars}}.
'{STRCHAR}+'            :       {token, {string, TokenLine, parse_string(TokenChars)}}.
''                      :       {token, {string, TokenLine, ""}}.
{                       :       {token, {lcurly, TokenLine, TokenChars}}.
}                       :       {token, {rcurly, TokenLine, TokenChars}}.
\[                      :       {token, {lbrace, TokenLine, TokenChars}}.
\]                      :       {token, {rbrace, TokenLine, TokenChars}}.
\(                      :       {token, {lparen, TokenLine, TokenChars}}.
\)                      :       {token, {rparen, TokenLine, TokenChars}}.
\@                      :       {token, {at, TokenLine, TokenChars}}.
\,                      :       {token, {comma, TokenLine, TokenChars}}.

Erlang code.
parse_string(Chars) ->
  StringEnd = length(Chars) - 2,
  lists:sublist(Chars, 2, StringEnd).
