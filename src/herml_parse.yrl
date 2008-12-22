Nonterminals
tag_decl tag_stem id_attr class_attr attr_list attrs attr string
chr_list name name_list var_ref fun_call shortcuts
class_list iter_generator iter_generator_list iter
template_stmt.

Terminals
tag_start class_start id_start number
lcurly rcurly lbrace rbrace lparen rparen
at comma quote chr colon slash
text dash lt pipe space bang.

Rootsymbol template_stmt.

iter_generator -> name lt dash var_ref : {iter_generator, '$4', '$1'}.

iter_generator_list -> iter_generator : ['$1'].
iter_generator_list -> iter_generator comma iter_generator_list : ['$1'|'$3'].

iter -> lbrace text pipe pipe iter_generator_list rbrace : {loop, {template, unwrap('$2')}, {generators, '$5'}}.

var_ref -> at name : {var_ref, unwrap('$2')}.
fun_call -> at name colon name : {fun_call, name_to_atom('$2'), name_to_atom('$4')}.

string -> quote chr_list quote : {string, '$2'}.
string -> quote quote : {string, ""}.

chr_list -> chr : unwrap('$1').
chr_list -> number :  number_to_list('$1').
chr_list -> tag_start : "%".
chr_list -> chr chr_list : unwrap('$1') ++ '$2'.
chr_list -> number chr_list : number_to_list('$1') ++ '$2'.
chr_list -> tag_start chr_list : "%" ++ '$2'.
chr_list -> class_start : ".".
chr_list -> class_start chr_list : "." ++ '$2'.
chr_list -> id_start : "#".
chr_list -> id_start chr_list : "#" ++ '$2'.
chr_list -> lparen : "(".
chr_list -> lparen chr_list : "(" ++ '$2'.
chr_list -> rparen : ")".
chr_list -> rparen chr_list : ")" ++ '$2'.
chr_list -> colon : ":".
chr_list -> colon chr_list : ":" ++ '$2'.
chr_list -> space : unwrap('$1').
chr_list -> space chr_list : unwrap('$1') ++ '$2'.

name -> name_list : {name, '$1'}.

name_list -> chr : unwrap('$1').
name_list -> chr name_list : unwrap('$1') ++ '$2'.

id_attr -> id_start name : unwrap_label_attr(id, '$2').
id_attr -> id_start number : unwrap_label_attr(id, '$2').

class_attr -> class_start name : unwrap_label_attr(class, '$2').

attr_list -> lbrace rbrace : [].
attr_list -> lbrace attrs rbrace : '$2'.
attr_list -> lbrace fun_call rbrace : ['$2'].
attr_list -> lbrace fun_call comma attrs rbrace : ['$2'|'$4'].
attr_list -> lbrace fun_call comma space attrs rbrace : ['$2'|'$5'].

attrs -> attr : ['$1'].
attrs -> attr comma attrs : ['$1'] ++ '$3'.
attrs -> attr comma space attrs : ['$1'] ++ '$4'.

attr -> lcurly name comma string rcurly : {name_to_atom('$2'), unwrap('$4')}.
attr -> lcurly name comma var_ref rcurly : {name_to_atom('$2'), '$4'}.

attr -> lcurly name comma space string rcurly : {name_to_atom('$2'), unwrap('$5')}.
attr -> lcurly name comma space var_ref rcurly : {name_to_atom('$2'), '$5'}.


template_stmt -> tag_decl : '$1'.
template_stmt -> iter : '$1'.

%% raw variable ref
tag_decl -> var_ref : '$1'.

%% doctype selector
tag_decl -> bang bang bang : {doctype, "Transitional"}.
tag_decl -> bang bang bang space : {doctype, "Transitional"}.
tag_decl -> bang bang bang space chr_list : {doctype, '$5'}.

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

%% id and class shortcuts
shortcuts -> id_attr class_list : ['$1'|'$2'].
shortcuts -> id_attr : ['$1'].
shortcuts -> class_list id_attr : ['$2'|'$1'].
shortcuts -> class_list : '$1'.

class_list -> class_attr class_list : ['$1'|'$2'].
class_list -> class_attr : ['$1'].

Erlang code.
unwrap_label_attr(Label, {_, Value}) ->
  {Label, Value}.

unwrap({text, _, Value}) ->
  Value;
unwrap({chr, _, Value}) ->
  Value;
unwrap({string, Value}) ->
  Value;
unwrap({space, _, Value}) ->
  Value;
unwrap({name, Value}) ->
  Value.

number_to_list({number, _, Value}) ->
  integer_to_list(Value).

name_to_atom({name, Value}) ->
  list_to_atom(Value).
