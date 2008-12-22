Definitions.

WS  = [\s|\t|\n|\r]
D = [0-9]
STRCHAR = [a-z|A-Z|0-9|_|]
BANG = [!]

Rules.

{WS}+                   :       skip_token.
\-			:	{token, {dash, TokenLine, TokenChars}}.
\%                      :       {token, {tag_start, TokenLine, TokenChars}}.
\.                      :       {token, {class_start, TokenLine, TokenChars}}.
#                       :       {token, {id_start, TokenLine, TokenChars}}.
{D}+                    :       {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{STRCHAR}+		:	{token, {chr, TokenLine, TokenChars}}.
{                       :       {token, {lcurly, TokenLine, TokenChars}}.
}                       :       {token, {rcurly, TokenLine, TokenChars}}.
\[                      :       {token, {lbrace, TokenLine, TokenChars}}.
\]                      :       {token, {rbrace, TokenLine, TokenChars}}.
\@                      :       {token, {at, TokenLine, TokenChars}}.
\,                      :       {token, {comma, TokenLine, TokenChars}}.
'			: 	{token, {quote, TokenLine, TokenChars}}.
\:			:	{token, {colon, TokenLine, TokenChars}}.
\/			:	{token, {slash, TokenLine, TokenChars}}.
{BANG}{BANG}{BANG}      :       {token, {doctype_start, TokenLine, TokenChars}}.
\(                      :       {token, {lparen, TokenLine, TokenChars}}.
\)                      :       {token, {rparen, TokenLine, TokenChars}}.
|			:	{token, {pipe, TokenLine, TokenChars}}.
<			:	{token, {lt, TokenLine, TokenChars}}.
>			:	{token, {gt, TokenLine, TokenChars}}.

Erlang code.
%% Not used