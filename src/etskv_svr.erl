-module(etskv_svr).

-behaviour(gen_server).

%% entry point / supervisor callback
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("etskv/include/types.hrl").

start_link() ->
    #{store := Name, options := Options} = etskv_util:env(),
    gen_server:start_link({local, Name}, ?MODULE, [Name, Options], []).

init([Name, _Options]) ->
    process_flag(trap_exit, true),
    case ets:info(Name, name) of
        undefined ->
            ets:new(Name, [ordered_set, public, named_table,
                           {read_concurrency, true},
                           {write_concurrency, true}]);
        _ ->
            ok
    end,
    Version = case ets:insert_new(Name, {'$version', 0}) of
                  true -> 0;
                  false ->
                      ets:update_counter(Name, '$version', {2, 0})
              end,

    {ok, #{tab => Name,
           version => Version,
           iterator => nil,
           iterators => #{},
           busy_versions => []}}.

handle_call({batch, Ops}, _From, State) ->
    #{ tab := Tab, version := Version} = State,
    NextVersion = Version + 1,
    ToInsert = process_ops(Ops, NextVersion, Tab, []),
    %% the operations is atomic,
    ets:insert(Tab, ToInsert),
    {reply, ok, State#{ version => NextVersion }};

handle_call({new_iterator, Options}, _From, State) ->
    {Itr, NewState} = etskv_iter:spawn(State, Options) ,
    {reply, Itr, NewState};

handle_call(close, _From, #{ tab := Tab } = State) ->
    ets:delete(Tab),
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, badarg, State}.

handle_cast({batch, Ops}, State) ->
    #{ tab := Tab, version := Version} = State,
    NextVersion = Version +1,
    ToInsert = process_ops(Ops, NextVersion, Tab, []),
    ets:insert(Tab, ToInsert),
    {noreply, State#{ version => NextVersion }};

handle_cast({close_iterator, Itr}, State) ->
    catch exit(Itr, normal),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State) ->
    #{ iterators := Iterators, locks_versions := Locks} = State,
    Iterators2 = maps:remove(Pid, Iterators),
    Locks2 = case maps:find(Pid, Iterators) of
                 {ok, Version} -> etskv_util:unlock_version(Version, Locks);
                 error -> Locks
             end,
    {noreply, State#{iterators => Iterators2, locks_versions => Locks2}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
      {ok, State}.

%%% Private functions

process_ops([{put, Key, Value} | Rest], Version, Tab, Acc) ->
    Meta = case ets:lookup(Tab, {k, Key}) of
               [] -> {Version, Version, 1};
               [{_, {XMin, _, _}}] -> {XMin, Version, 1}
           end,

    Acc2 = [{{r, etskv_util:make_key(Key, Version, 1)}, Value},
            {{k, Key}, Meta}| Acc],
    process_ops(Rest, Version, Tab, Acc2);
process_ops([{delete, Key} | Rest], Version, Tab, Acc) ->
    process_ops([{del, Key} | Rest], Version, Tab, Acc);
process_ops([{del, Key} | Rest], Version, Tab, Acc) ->
    case ets:lookup(Tab, {k, Key}) of
        [] ->
            process_ops(Rest, Version, Tab, Acc);
        [{_, {XMin, _, _}}] ->
            Acc2 = [{{r, etskv_util:make_key(Key, Version, 0)}, <<>>},
                    {{k, Key}, {XMin, Version, 0}}],
            process_ops(Rest, Version, Tab, Acc2)
    end;
process_ops([], Version, _Tab, Acc) ->
    lists:reverse([{'$version', Version} | Acc]).
