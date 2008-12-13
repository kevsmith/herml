Nonterminals
tag_decl id_attr class_attr attr_list attrs attr string
char_list name name_list.

Terminals
tag_start class_start id_start number
lcurly rcurly lbrace rbrace lparen
rparen at comma quote char.

Rootsymbol tag_decl.

string -> quote char_list quote : {string, '$2'}.
string -> quote quote : {string, ""}.

char_list -> char : unwrap_char('$1').
char_list -> number :  number_to_list('$1').
char_list -> char char_list : unwrap_char('$1') ++ '$2'.
char_list -> number char_list : number_to_list('$1') ++ '$2'.
char_list -> tag_start char_list : "%" ++ '$2'.

name -> name_list : {name, '$1'}.

name_list -> char : unwrap_char('$1').
name_list -> char name_list : unwrap_char('$1') ++ '$2'.

id_attr -> id_start name : unwrap_label_attr(id, '$2').
id_attr -> id_start number : unwrap_label_attr(id, '$2').

class_attr -> class_start name : unwrap_label_attr(class, '$2').

attr_list -> lbrace rbrace : [].
attr_list -> lbrace attrs rbrace : '$2'.

attrs -> attr : ['$1'].
attrs -> attr comma attrs : ['$1'] ++ '$3'.

attr -> lcurly name comma string rcurly : {name_to_atom('$2'), unwrap_string('$4')}.

%% named tags
tag_decl -> tag_start name : {tag_decl, [unwrap_label_attr(tag_name, '$2')]}.
tag_decl -> tag_start name id_attr : {tag_decl, [unwrap_label_attr(tag_name, '$2'), '$3']}.
tag_decl -> tag_start name class_attr: {tag_decl, [unwrap_label_attr(tag_name, '$2'), '$3']}.
tag_decl -> tag_start name id_attr class_attr : {tag_decl, [unwrap_label_attr(tag_name, '$2'), '$3', '$4']}.
tag_decl -> tag_start name class_attr id_attr : {tag_decl, [unwrap_label_attr(tag_name, '$2'), '$4', '$3']}.

%% named tags w/attribute lists
tag_decl -> tag_start name attr_list : {tag_decl, lists:append([unwrap_label_attr(tag_name, '$2')], '$3')}.
tag_decl -> tag_start name id_attr attr_list : {tag_decl, lists:append([unwrap_label_attr(tag_name, '$2'), '$3'], '$4')}.
tag_decl -> tag_start name class_attr attr_list: {tag_decl, lists:append([unwrap_label_attr(tag_name, '$2'), '$3'], '$4')}.
tag_decl -> tag_start name id_attr class_attr attr_list : {tag_decl, lists:append([unwrap_label_attr(tag_name, '$2'), '$3', '$4'], '$5')}.
tag_decl -> tag_start name class_attr id_attr attr_list : {tag_decl, lists:append([unwrap_label_attr(tag_name, '$2'), '$4', '$3'], '$5')}.

%% default div tags
tag_decl -> id_attr : {tag_decl, [{tag_name, "div"}, '$1']}.
tag_decl -> class_attr : {tag_decl, [{tag_name, "div"}, '$1']}.
tag_decl -> id_attr class_attr : {tag_decl, [{tag_name, "div"}, '$1', '$2']}.
tag_decl -> class_attr id_attr : {tag_decl, [{tag_name, "div"}, '$2', '$1']}.

%% default div tags w/attribute_lists
tag_decl -> id_attr attr_list : {tag_decl, lists:append([{tag_name, "div"}, '$1'], '$2')}.
tag_decl -> class_attr attr_list : {tag_decl, lists:append([{tag_name, "div"}, '$1'], '$2')}.
tag_decl -> id_attr class_attr attr_list : {tag_decl, lists:append([{tag_name, "div"}, '$1', '$2'], '$3')}.
tag_decl -> class_attr id_attr attr_list : {tag_decl, lists:append([{tag_name, "div"}, '$2', '$1'], '$3')}.

Erlang code.
unwrap_label_attr(Label, {_, Value}) ->
  {Label, Value}.

unwrap_char({char, _, Value}) ->
  Value.

unwrap_string({string, Value}) ->
  Value.

number_to_list({number, _, Value}) ->
  integer_to_list(Value).

name_to_atom({name, Value}) ->
  list_to_atom(Value).
