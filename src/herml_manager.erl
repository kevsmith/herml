-module(herml_manager).

-author("kevin@hypotheticalabs.com").

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/2, start_link/3, execute_template/2, execute_template/3]).
-export([execute_template/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {root_dir,
         table_name,
         devmode}).

-record(cache_entry,
        {file_path,
         fingerprint,
         template}).

start_link(Name, RootDir) when is_atom(Name),
                               is_list(RootDir) ->
  start_link(Name, RootDir, []).

start_link(Name, RootDir, Options) when is_atom(Name),
                                        is_list(RootDir) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, RootDir, Options], []).

execute_template(ManagerName, TemplatePath) ->
  execute_template(ManagerName, TemplatePath, []).

execute_template(ManagerName, TemplatePath, Env) ->
  gen_server:call(ManagerName, {exec_template, TemplatePath, Env, 0}).

execute_template(ManagerName, TemplatePath, Env, Offset) ->
  gen_server:call(ManagerName, {exec_template, TemplatePath, Env, Offset}).


init([EtsTableName, RootDir, Options]) ->
  case verify_dir(RootDir) of
    {error, Reason} ->
      {stop, Reason};
    {ok, FinalRootDir} ->
      process_flag(trap_exit, true),
      ets:new(EtsTableName, [private, named_table, {keypos, 2}]),
      {ok, #state{root_dir=FinalRootDir,
                  table_name=EtsTableName,
                  devmode=proplists:get_value(development, Options, false)}}
  end.

handle_call({exec_template, TemplatePath, Env, Offset}, From, State) ->
  Content = case ets:match(State#state.table_name,
                           {cache_entry, TemplatePath, '$2', '$1'}, 1) of
              '$end_of_table' ->
                load_and_store(State#state.table_name,
                               State#state.root_dir,
                               TemplatePath);
              {[[C, Fingerprint]], '$end_of_table'} ->
                if
                  State#state.devmode ->
                    case read_fingerprint(State#state.root_dir, TemplatePath) /= Fingerprint of
                      true ->
                        load_and_store(State#state.table_name,
                                       State#state.root_dir,
                                       TemplatePath);
                      false ->
                        C
                    end;
                  true ->
                    C
                end
            end,
  case Content of
    {error, _} ->
      {reply, Content, State};
    _ ->
      NewEnv = prep_env(Env, State),
      spawn(fun () -> exec_template(Content, NewEnv, Offset, From) end),
      {noreply, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  ets:delete(State#state.table_name).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
prep_env(Env, State) ->
  case proplists:get_value("__herml_manager__", Env) of
    undefined ->
      [{"__herml_manager__", State#state.table_name}|Env];
    _ ->
      Env
  end.

exec_template(Template, Env, Offset, From) ->
  Output = herml_htmlizer:render(Template, Env, Offset),
  gen_server:reply(From, {ok, Output}).

load_and_store(TableName, RootDir, TemplatePath) ->
  case fetch_file(RootDir, TemplatePath) of
    {ok, Fingerprint, Content} ->
      case herml_parser:string(Content) of
        {error, Error} ->
          {error, Error};
        CT ->
          ets:insert(TableName, #cache_entry{file_path=TemplatePath, fingerprint=Fingerprint, template=CT}),
          CT
      end;
    Error ->
      Error
  end.

fetch_file(RootDir, FilePath) ->
  {ok, Content} = file:read_file(filename:join(RootDir, FilePath)),
  {ok, read_fingerprint(RootDir, FilePath), Content}.

read_fingerprint(RootDir, FilePath) ->
  {ok, FileInfo} = file:read_file_info(filename:join(RootDir, FilePath)),
  #file_info{mtime=MTime, size=Size} = FileInfo,
  calendar:datetime_to_gregorian_seconds(MTime) + Size.

verify_dir(D) ->
  case filelib:is_dir(D) of
    true ->
      {ok, D};
    false ->
      {error, {bad_dir, D}}
  end.
