{application, herml,
 [
  {description, "Haiku markup for Erlang"},
  {vsn, "0.1"},
  {modules, [herml_htmlizer,
             herml_manager,
             herml_parse,
             herml_parser,
             herml_reader,
             herml_scan]},
   {registered, []},
   {env, []},
   {applications, [kernel, stdlib]}]}.
