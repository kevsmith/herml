Nonterminals
  template_stmt
    tag_decl
      tag_stem
        shortcuts id_attr class_attr class_list
        attr_list attrs attr attr_key attr_value
    var_ref
    fun_call
      param param_list param_value
    iter
      iter_list iter_item
    doctype
      doctype_name doctype_name_elem
    list_sep
    list_open list_close
    tuple_open tuple_close
    params_open params_close
    name.

Terminals
  number chr string
  tag_start
  class_start
  id_start
  lcurly rcurly
  lbrace rbrace
  lparen rparen
  at
  comma
  colon
  slash
  dash lt
  bang
  underscore
  space.

Rootsymbol template_stmt.

template_stmt -> doctype : '$1'.
template_stmt -> var_ref : '$1'.
template_stmt -> iter : '$1'.
template_stmt -> fun_call : '$1'.
template_stmt -> tag_decl : '$1'.

%% doctype selector
doctype -> bang bang bang : {doctype, "Transitional", []}.
doctype -> bang bang bang space : {doctype, "Transitional", []}.
doctype -> bang bang bang space doctype_name : {doctype, '$5', []}.
doctype -> bang bang bang space doctype_name space doctype_name : {doctype, '$5', '$7'}.

doctype_name -> doctype_name_elem doctype_name : '$1' ++ '$2'.
doctype_name -> doctype_name_elem : '$1'.

doctype_name_elem -> chr : unwrap('$1').
doctype_name_elem -> dash : "-".
doctype_name_elem -> class_start : ".".
doctype_name_elem -> number : number_to_list('$1').

%% Variable reference for emitting, iterating, and passing to funcalls
var_ref -> at name : {var_ref, unwrap('$2')}.

%% Iterator
iter -> dash space list_open iter_item list_close space lt dash space var_ref : {iter, '$4', '$10'}.

iter_list -> iter_item : ['$1'].
iter_list -> iter_item list_sep iter_list : ['$1'|'$3'].

iter_item -> underscore : ignore.
iter_item -> var_ref : '$1'.
iter_item -> tuple_open iter_list tuple_close: {tuple, '$2'}.
iter_item -> list_open iter_list list_close: {list, '$2'}.

%% Function calls
fun_call -> at name colon name params_open params_close : {fun_call, name_to_atom('$2'), name_to_atom('$4'), []}.
fun_call -> at name colon name params_open param_list params_close : {fun_call, name_to_atom('$2'), name_to_atom('$4'), '$6'}.
fun_call -> at name colon name : {fun_call, name_to_atom('$2'), name_to_atom('$4'), []}.

fun_call -> at at name colon name params_open params_close : {fun_call_env, name_to_atom('$3'), name_to_atom('$5'), []}.
fun_call -> at at name colon name params_open param_list params_close : {fun_call_env, name_to_atom('$3'), name_to_atom('$5'), '$7'}.
fun_call -> at at name colon name : {fun_call_env, name_to_atom('$3'), name_to_atom('$5'), []}.

param_list -> param : ['$1'].
param_list -> param list_sep param_list : ['$1'|'$3'].

param -> param_value space : '$1'.
param -> param_value : '$1'.

param_value -> var_ref : '$1'.
param_value -> string : unwrap_param('$1').
param_value -> number : unwrap_param('$1').

%% Tag declarations
%% singletons or containers
tag_decl -> tag_stem : {tag_decl, '$1'}.
tag_decl -> tag_stem slash : {tag_decl, [{singleton, true}|'$1']}.

%% tag stems (names, ids, classes, attrs)
tag_stem -> tag_start name : [unwrap_label_attr(tag_name, '$2')].
tag_stem -> tag_start name shortcuts : [unwrap_label_attr(tag_name, '$2')|'$3'].
tag_stem -> tag_start name attr_list : lists:append([unwrap_label_attr(tag_name, '$2')], '$3').
tag_stem -> tag_start name shortcuts attr_list : lists:flatten([unwrap_label_attr(tag_name, '$2'), '$3', '$4']).
tag_stem -> shortcuts : [{tag_name, "div"}|'$1'].
tag_stem -> shortcuts attr_list : lists:append([{tag_name, "div"}|'$1'],'$2').

%% id and class shortcuts - only one id allowed
shortcuts -> id_attr class_list : ['$1'|'$2'].
shortcuts -> id_attr : ['$1'].
shortcuts -> class_list id_attr : ['$2'|'$1'].
shortcuts -> class_list : '$1'.

%% Class shortcuts - multiple allowed
class_list -> class_attr class_list : ['$1'|'$2'].
class_list -> class_attr : ['$1'].

%% Id shortcut
id_attr -> id_start name : unwrap_label_attr(id, '$2').
id_attr -> id_start number : unwrap_label_attr(id, '$2').

%% Class shortcut
class_attr -> class_start name : unwrap_label_attr(class, '$2').

%% Generic attribute lists
attr_list -> list_open list_close : [].
attr_list -> list_open attrs list_close : '$2'.
attr_list -> list_open fun_call list_close : ['$2'].
attr_list -> list_open fun_call list_sep attrs list_close : ['$2'|'$4'].

attrs -> attr : ['$1'].
attrs -> attr list_sep attrs : ['$1'] ++ '$3'.

attr -> attr space : '$1'.
attr -> tuple_open attr_key list_sep attr_value tuple_close : {'$2', '$4'}.

attr_key -> attr_key space : '$1'.
attr_key -> string : list_to_atom(unwrap('$1')).
attr_key -> name : name_to_atom('$1').
attr_key -> var_ref : '$1'.
attr_key -> fun_call : '$1'.

attr_value -> attr_value space : '$1'.
attr_value -> string : unwrap('$1').
attr_value -> number : integer_to_list(unwrap('$1')).
attr_value -> name : unwrap('$1').
attr_value -> var_ref : '$1'.
attr_value -> fun_call : '$1'.

%% Space insensitive parens
params_open -> lparen space : '$1'.
params_open -> lparen : '$1'.
params_close -> rparen : '$1'.

%% Space insensitive square-brackets
list_open -> lbrace space : '$1'.
list_open -> lbrace : '$1'.
list_close -> rbrace : '$1'.

%% Space insensitive curly-brackets
tuple_open -> lcurly space : '$1'.
tuple_open -> lcurly : '$1'.
tuple_close -> rcurly : '$1'.

%% Space-insensitive comma
list_sep -> comma space : '$1'.
list_sep -> comma : '$1'.

%% Simple identifiers / atoms
name -> chr : {name, unwrap('$1')}.

Erlang code.
unwrap_label_attr(Label, {_, Value}) ->
  {Label, Value}.
unwrap_param({string, _, Value}) ->
  {string, Value};
unwrap_param({number, _, Value}) ->
  {number, Value}.

unwrap({text, _, Value}) ->
  Value;
unwrap({chr, _, Value}) ->
  Value;
unwrap({string, _, Value}) ->
  Value;
unwrap({space, _, Value}) ->
  Value;
unwrap({number, _, Value}) ->
  Value;
unwrap({name, Value}) ->
  Value.

number_to_list({number, _, Value}) ->
  integer_to_list(Value).

name_to_atom({name, Value}) ->
  list_to_atom(Value).
