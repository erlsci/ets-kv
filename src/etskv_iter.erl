-module(etskv_iter).

-export([new/1, new/2,
         move/2,
         close/1,
         loop/1,
         spawn/2]).

-include_lib("etskv/include/types.hrl").

%% @doc initialize an iterator. An iterator allows you to iterate over a consistent view of the store without
%% blocking any writes.
%%
%% Note: compared to ETS you won't have to worry about the possibility that a key may be inserted while you iterrate.
%% The version you browsing won't be affected.
%%
%% Used to be iterator(Store)
-spec new(Store :: store()) -> iterator().
new(Store) ->
    new(Store, []).

%% @doc initialize an iterator with options. `keys_only' is the only option available for now.
-spec new(Store :: store(), Options :: iterator_options()) -> iterator().
new(#{ writer := W }, Options) ->
    gen_server:call(W, {new_iterator, Options}).

%% @doc traverse the iterator using different operations
%%
%% Used to be iterator_move(Iterator, Op)
-spec move(Iterator :: iterator(), Op :: iterator_ops()) ->
    {ok, Key :: key(), Value :: value()}
    | {ok, Key :: key()}
    | '$iterator_limit'
    | {error, iterator_closed}
    | {error, invalid_iterator}.
move(Itr, Op)
  when Op =:= first;
       Op =:= last;
       Op =:= next;
       Op =:= prev;
       is_binary(Op) ->

	Tag = erlang:monitor(process, Itr),

	catch Itr ! {Tag, self(), Op},
	receive
		{Tag, Reply} ->
			erlang:demonitor(Tag, [flush]),
			Reply;
		{'DOWN', Tag, _, Itr, _} ->
			{error, iterator_closed}
	after 5000 ->
			erlang:demonitor(Tag, [flush]),
			exit(timeout)
	end;
move(_, _) ->
	error(badarg).

%% @doc close the iterator
%%
%% Used to be iterator_close(Iterator)
-spec close(Iterator :: iterator()) -> ok.
close(Itr) ->
    MRef = erlang:monitor(process, Itr),
    catch Itr ! {MRef, self(), close},
    receive
        {MRef, ok} ->
            erlang:demonitor(MRef, [flush]),
            ok;
        {'DOWN', MRef, _, _} -> ok
    after 5000 ->
              erlang:demonitor(MRef, [flush]),
              error(timeout)
    end.

%% Used to be iterator_loop(Itr)
loop(Itr) ->
    receive
        {Ref, From, close} ->
            catch From ! {Ref, ok};
        {Ref, From, first} ->
            {Reply, Itr2} = iterator_first(Itr),
            From ! {Ref, Reply},
            loop(Itr2);
        {Ref, From, last} ->
            {Reply, Itr2} = iterator_last(Itr),
            From ! {Ref, Reply},
            loop(Itr2);
        {Ref, From, next} ->
            {Reply, Itr2} = iterator_next(Itr),
            From ! {Ref, Reply},
            loop(Itr2);
        {Ref, From, prev} ->
            {Reply, Itr2} = iterator_prev(Itr),
            From ! {Ref, Reply},
            loop(Itr2);
        {Ref, From, Key} when is_binary(Key) ->
            {Reply, Itr2} = iterator_next_key(Key, Itr),
            From ! {Ref, Reply},
            loop(Itr2)
    end.

spawn(State, Options) ->
    #{version := Version,
      iterator := Itr,
      iterators := Iterators,
      busy_versions := Locks} = State,

    KeysOnly = proplists:get_value(keys_only, Options, false),
    {Itr2, State2} = maybe_init_iterator(Itr, State),
    Pid = spawn_link(?MODULE, loop, [Itr2#{keys_only => KeysOnly}]),
    Iterators2 = Iterators#{Pid => Version},
    Locks2 = etskv_util:lock_version(Version, Locks),
    NewState = State2#{iterators => Iterators2,
                       locks_versions => Locks2},
    {Pid, NewState}.

%%% Private functions

maybe_init_iterator(nil, State) ->
    init_iterator(State);
maybe_init_iterator(#{version := V}=Itr, #{version := V}=State) ->
    {Itr, State};
maybe_init_iterator(_, State) ->
    init_iterator(State).

init_iterator(#{tab := Tab, version := Version}=State) ->
    Itr = #{tab => Tab,
            version => Version,
            dir => fwd,
            next => nil},
    {Itr, State#{iterator => Itr}}.

iterator_first(Itr) ->
    iterator_next(Itr#{dir => fwd, next => nil}).

iterator_last(Itr) ->
    {Reply, Itr2} = iterator_prev(Itr#{dir => rev, last => nil, next => nil}),
    {Reply, Itr2#{dir => fwd}}.

iterator_lookup('$end_of_table', Itr) ->
    {'$iterator_limit', Itr#{last => '$iterator_limit'}};
iterator_lookup({[{Key, _XMax}], Cont}, #{keys_only := true}=Itr) ->
    {{ok, Key}, Itr#{ last => Key, next => Cont }};
iterator_lookup({[{Key, XMax}], Cont}, #{tab := Tab, version := Version}=Itr) ->
    StoreVersion = erlang:min(Version, XMax),
    case ets:lookup(Tab, {r, etskv_util:make_key(Key, StoreVersion, 1)}) of
        [] -> error({error, invalid_iterator});
        [{_, Value}] ->
            {{ok, Key, Value}, Itr#{ last => Key, next => Cont }}
    end.

iterator_next(#{ tab := Tab, version := Version, next := nil } = Itr) ->
    Next = ets:select(Tab, [{{{k, '$1'}, {'$2', '$3', '$4'}},
                             [{'andalso',
                               {'=<', '$2', Version},
                               {'==', '$4', 1}}],
                             [{{'$1', '$3'}}]}], 1),
    iterator_lookup(Next, Itr#{ dir => fwd} );

iterator_next(#{ dir := fwd, next := Cont }=Itr) ->
    Next = ets:select(Cont),
    iterator_lookup(Next, Itr);
iterator_next(#{ dir := fwd, last := '$iterator_limit' }=Itr) ->
    {'$iterator_limit', Itr};
iterator_next(#{ last := '$iterator_limit' }=Itr) ->
    iterator_next(Itr#{ dir => fwd, last => nil, next => nil });
iterator_next(#{ tab := Tab, version := Version, last := Last }=Itr) ->
    Next = ets:select(Tab, [{{{k, '$1'}, {'$2', '$3', '$4'}},
                             [{'andalso',
                               {'>', '$1', Last },
                               {'=<', '$2', Version},
                               {'==', '$4', 1}}],
                             [{{'$1', '$3'}}]}], 1),
    iterator_lookup(Next, Itr#{ dir => fwd} ).

iterator_prev(#{ tab := Tab, version := Version, next := nil } = Itr) ->
    Prev = ets:select_reverse(Tab, [{{{k, '$1'}, {'$2', '$3', '$4'}},
                                     [{'andalso',
                                       {'=<', '$2', Version},
                                       {'==', '$4', 1}}],
                                     [{{'$1', '$3'}}]}], 1),
    iterator_lookup(Prev, Itr#{ dir => rev} );
iterator_prev(#{ dir := rev, next := Cont }=Itr) ->
    Prev = ets:select(Cont),
    iterator_lookup(Prev, Itr);
iterator_prev(#{ dir := rev, last := '$iterator_limit' }=Itr) ->
    {'$iterator_limit', Itr};
iterator_prev(#{ last := '$iterator_limit' }=Itr) ->
    iterator_prev(Itr#{ dir => rev, last => nil, next => nil });
iterator_prev(#{ tab := Tab, version := Version, last := Last }=Itr) ->
    Prev = ets:select_reverse(Tab, [{{{k, '$1'}, {'$2', '$3', '$4'}},
                                     [{'andalso',
                                       {'<', '$1', Last },
                                       {'=<', '$2',  Version},
                                       {'==', '$4',  1}}],
                                     [{{'$1', '$3'}}]}], 1),
    iterator_lookup(Prev, Itr#{ dir => fwd} ).

iterator_next_key(Key, #{ tab := Tab, version := Version} = Itr) ->
    Next = ets:select(Tab, [{{{k, '$1'}, {'$2', '$3', '$4'}},
                             [{'andalso',
                               {'>=', '$1', Key },
                               {'=<', '$2', Version},
                               {'==', '$4', 1}}],
                             [{{'$1', '$3'}}]}], 1),
    iterator_lookup(Next, Itr#{ dir => fwd} ).
