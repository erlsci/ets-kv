%%-*- mode: erlang -*-
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(etskv).

%% API exports
-export([find/2,
         get/2, get/3,
         put/3,
         delete/2,
         batch/2,
         contains/2,
         fold_keys/4,
         fold/4,
         open/1, open/2,
         close/1]).

-define(DEFAULT_FOLD_OPTIONS, #{start_key => first,
                                end_key => nil,
                                gt => nil,
                                gte => nil,
                                lt => nil,
                                lte => nil,
                                max => 0}).

-include_lib("etskv/include/types.hrl").

%%% API functions

find(Key, Store) ->
    case catch get_last(Key, Store) of
        not_found -> error;
        V -> {ok, V}
    end.

%% @doc returns the Value associated to the Key. If the key doesn't exits and error will be raised.
-spec get(Key::key(), Store :: store()) -> Value :: value().
get(Key, Store) ->
   get_last(Store, Key).

%% @doc returns the Value associated to the Key. Default is returned if the Key doesn't exist.
-spec get(Key :: key(), Store :: store(), Default :: any()) -> Value :: value().
get(Key, Store, Default) ->
    case etskv:find(Key, Store) of
        {ok, Val} -> Val;
        error -> Default
    end.

%% @doc Associates Key with value Value and store it.
-spec put(Key :: key(), Value :: value(), Store :: store()) -> ok.
put(Key, Value, Store) ->
    batch([{put, Key, Value}], Store).

%% @doc delete a Key
-spec delete(Key :: key(), Store :: store()) -> ok.
delete(Key, Store) ->
    batch([{del, Key}], Store).

%% @doc Apply atomically a set of updates to the store
-spec batch(Ops :: batch_ops(), Store :: store()) -> ok.
batch(Ops, #{ writer := W }) ->
    gen_server:call(W, {batch, Ops}).

%% @doc check if a Key exists in the store
-spec contains(Key :: key(), Store :: store()) -> true | false.
contains(Key, #{ tab := Tab }) ->
    case ets:next(Tab, {r, etskv_util:prefix(Key)}) of
        '$end_of_table' -> false;
        {r, KeyBin} ->
            {Key, _, Type} = etskv_util:decode_key(KeyBin),
            Type =:= 1
    end.

fold_keys(Fun, Acc0, Store, Opts0) ->
    Itr = etskv_iter:new(Store, [keys_only]),
    Opts = fold_options(Opts0, ?DEFAULT_FOLD_OPTIONS),
    do_fold(Itr, Fun, Acc0, Opts).

%% @doc fold all K/Vs in the store with a function Fun.
%% Additionnaly you can pass the following options:
%% <ul>
%% <li>'gt', (greater than), 'gte' (greather than or equal): define the lower
%% bound of the range to fold. Only the records where the key is greater (or
%% equal to) will be given to the function.</li>
%% <li>'lt' (less than), 'lte' (less than or equal): define the higher bound
%% of the range to fold. Only the records where the key is less than (or equal
%% to) will be given to the function</li>
%% <li>'start_key', 'end_key', legacy to 'gte', 'lte'</li>
%% <li>'max' (default=0), the maximum of records to fold before returning the
%% resut</li>
%% <li>'fill_cache' (default is true): should be the data cached in
%% memory?</li>
%% </ul>
%%
%% Example of function : Fun(Key, Value, Acc) -> Acc2 end.
-spec fold(Fun :: function(), AccIn :: any(), Store :: store(), Opts :: fold_options()) -> AccOut :: any().
fold(Fun, Acc0, Store, Opts0) ->
    Itr = etskv_iter:new(Store, []),
    Opts = fold_options(Opts0, ?DEFAULT_FOLD_OPTIONS),
    do_fold(Itr, Fun, Acc0, Opts).

%% @doc open the store Name
-spec open(atom()) -> store().
open(Name) ->
    open(Name, []).

open(Name, Options) when is_atom(Name) ->
    {ok, Pid} = etskv_svr:start_link(Name, Options),
    #{ tab => Name, writer => Pid };
open(Name, _) ->
    error({invalid_name, Name}).

%% @doc close the store
close(#{ writer := Writer }) ->
    try
        gen_server:call(Writer, close, infinity)
    catch
        exit:{noproc,_} -> ok;
        exit:noproc -> ok;
        %%Handle the case where the monitor triggers
        exit:{normal, _} -> ok
    end.

%%% Private functions

get_last(#{ tab := Tab }, Key) ->
    case ets:next(Tab, {r, etskv_util:prefix(Key)}) of
        '$end_of_table' -> error(not_found);
        {r, KeyBin} ->
            {_Key, _Version, Type} = etskv_util:decode_key(KeyBin),
            if
                Type =:= 0 -> error(not_found); %% deleted
                true -> i_lookup(Tab, {r, KeyBin})
            end
    end.

i_lookup(Tab, Key) ->
    [{_, V}] = ets:lookup(Tab, Key),
    V.

do_fold(Itr, Fun, Acc0, #{gt := GT, gte := GTE}=Opts) ->
    {Start, Inclusive} = case {GT, GTE} of
                      {nil, nil} -> {first, true};
                      {first, _} -> {first, false};
                      {K, _} when is_binary(K) -> {K, false};
                      {_, K} -> {K, true}
                  end,

    try
        case etskv_iter:move(Itr, Start) of
            {ok, Start} when Inclusive /= true ->
                fold_loop(etskv_iter:move(Itr, next), Itr, Fun,
                          Acc0, 0, Opts);
            {ok, Start, _V} when Inclusive /= true ->
                fold_loop(etskv_iter:move(Itr, next), Itr, Fun,
                          Acc0, 0, Opts);
            Next ->
                fold_loop(Next, Itr, Fun, Acc0, 0, Opts)

        end
    after
        etskv_iter:close(Itr)
    end.

fold_loop('$iterator_limit', _Itr, _Fun, Acc, _N, _Opts) ->
	Acc;
fold_loop({error, iterator_closed}, _Itr, _Fun, Acc0, _N, _Opts) ->
    throw({iterator_closed, Acc0});
fold_loop({error, invalid_iterator}, _Itr, _Fun, Acc0, _N, _Opts) ->
    Acc0;
fold_loop({ok, K}=KO, Itr, Fun, Acc0, N0, #{lt := End}=Opts) when End /= nil, K < End ->
    fold_loop1(KO, Itr, Fun, Acc0, N0, Opts);
fold_loop({ok, K}=KO, Itr, Fun, Acc0, N0, #{lte := End}=Opts) when End /= nil orelse K =< End ->
    fold_loop1(KO, Itr, Fun, Acc0, N0, Opts);
fold_loop({ok, _K}=KO, Itr, Fun, Acc0, N,  #{lt := nil, lte := nil}=Opts) ->
    fold_loop1(KO, Itr, Fun, Acc0, N, Opts);
fold_loop({ok, _K}, _Itr, _Fun, Acc0, _N, _Opts) ->
    Acc0;
fold_loop({ok, K, _V}=KV, Itr, Fun, Acc0, N0, #{lt := End}=Opts) when End /= nil, K < End ->
    fold_loop1(KV, Itr, Fun, Acc0, N0, Opts);
fold_loop({ok, K, _V}=KV, Itr, Fun, Acc0, N0, #{lte := End}=Opts) when End /= nil, K =< End ->
    fold_loop1(KV, Itr, Fun, Acc0, N0, Opts);
fold_loop({ok, _K, _V}=KV, Itr, Fun, Acc0, N0, #{lt := nil, lte := nil}=Opts) ->
    fold_loop1(KV, Itr, Fun, Acc0, N0, Opts);
fold_loop({ok, _K, _V}, _Itr, _Fun, Acc0, _N, _Opts) ->
    Acc0.

fold_loop1({ok, K}, Itr, Fun, Acc0, N0, #{max := Max}=Opts) ->
    Acc = Fun(K, Acc0),
    N = N0 + 1,
    if ((Max =:=0) orelse (N < Max)) ->
            fold_loop(etskv_iter:move(Itr, next), Itr, Fun, Acc, N, Opts);
        true ->
            Acc
	end;
fold_loop1({ok, K, V}, Itr, Fun, Acc0, N0, #{max := Max}=Opts) ->
    Acc = Fun(K, V, Acc0),
    N = N0 + 1,
    if
        ((Max =:= 0) orelse (N < Max)) ->
            fold_loop(etskv_iter:move(Itr, next), Itr, Fun, Acc, N,  Opts);
        true ->
            Acc
    end.
fold_options([], Options) ->
    Options;
fold_options([{start_key, Start} | Rest], Options) when is_binary(Start) or (Start =:= first) ->
    fold_options(Rest, Options#{gte => Start});
fold_options([{end_key, End} | Rest], Options) when is_binary(End) or (End == nil) ->
    fold_options(Rest, Options#{lte=>End});
fold_options([{gt, GT} | Rest], Options) when is_binary(GT) or (GT =:= first) ->
    fold_options(Rest, Options#{gt=>GT});
fold_options([{gte, GT} | Rest], Options) when is_binary(GT) or (GT =:= first) ->
    fold_options(Rest, Options#{gte=>GT});
fold_options([{lt, LT} | Rest], Options) when is_binary(LT) or (LT == nil) ->
    fold_options(Rest, Options#{lt=>LT});
fold_options([{lte, LT} | Rest], Options) when is_binary(LT) or (LT == nil) ->
    fold_options(Rest, Options#{lte=>LT});
fold_options([{max, Max} | Rest], Options) ->
    fold_options(Rest, Options#{max=>Max});
fold_options([_ | Rest], Options) ->
    fold_options(Rest, Options).
