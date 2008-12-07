Nonterminals
tag_decl id_attr class_attr attr_list attrs attr.

Terminals
tag_start class_start id_start name number
string lcurly rcurly lbrace rbrace lparen
rparen at comma.

Rootsymbol tag_decl.

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
unwrap_label_attr(Label, {_, _, Value}) ->
  {Label, Value}.

unwrap_string({_, _, Value}) ->
  Value.

name_to_atom({_Name, _, Value}) ->
  list_to_atom(Value).
