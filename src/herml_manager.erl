-module(herml_manager).

-author("kevin@hypotheticalabs.com").

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/2, execute_template/2, execute_template/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {root_dir,
         table_name}).

-record(cache_entry,
        {file_path,
         timestamp,
         template}).

start_link(Name, RootDir) when is_atom(Name),
                               is_list(RootDir) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, RootDir], []).

execute_template(ManagerName, TemplatePath) ->
  execute_template(ManagerName, TemplatePath, []).

execute_template(ManagerName, TemplatePath, Env) ->
  gen_server:call(ManagerName, {exec_template, TemplatePath, Env}).

init([EtsTableName, RootDir]) ->
  case verify_dir(RootDir) of
    {error, Reason} ->
      {stop, Reason};
    {ok, FinalRootDir} ->
      process_flag(trap_exit, true),
      ets:new(EtsTableName, [private, named_table, {keypos, 2}]),
      {ok, #state{root_dir=FinalRootDir,
                  table_name=EtsTableName}}
  end.

handle_call({exec_template, TemplatePath, Env}, From, State) ->
  Content = case ets:match(State#state.table_name,
                           {cache_entry, TemplatePath, '$2', '$1'}, 1) of
              '$end_of_table' ->
                load_and_store(State#state.table_name,
                               State#state.root_dir,
                               TemplatePath);
              [[C, _], '$end_of_table'] ->
                C
            end,
  case Content of
    {error, _} ->
      {reply, Content, State};
    _ ->
      spawn(fun () -> exec_template(Content, Env, From) end),
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
exec_template(Template, Env, From) ->
  Output = herml_htmlizer:render(Template, Env),
  gen_server:reply(From, {ok, Output}).

load_and_store(TableName, RootDir, TemplatePath) ->
  case fetch_file(RootDir, TemplatePath) of
    {ok, Content} ->
      case herml_parser:string(Content) of
        {error, Error} ->
          {error, Error};
        CT ->
          ets:insert(TableName, #cache_entry{file_path=TemplatePath, timestamp=0, template=CT}),
          CT
      end;
    Error ->
      Error
  end.

fetch_file(RootDir, FilePath) ->
  file:read_file(RootDir ++ FilePath).

verify_dir(D) ->
  Dir = case string:right(D, 1) of
          "/" ->
            D;
          _ ->
            D ++ "/"
        end,
  case filelib:is_dir(Dir) of
    true ->
      {ok, Dir};
    false ->
      {error, {bad_dir, Dir}}
  end.
