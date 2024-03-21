-module(etskv_util).

-export([prefix/1,
         make_key/3,
         decode_key/1,
         lock_version/2,
         unlock_version/2]).

prefix(Key) ->
    << Key/binary, 16#ff >>.

make_key(Key, Version, Type) when is_binary(Key) ->
    << Key/binary, 16#FF, Type, (-Version):4/little-signed-integer-unit:8>>.

decode_key(Bin) when is_binary(Bin) ->
    case binary:split(Bin, << 16#ff >>) of
        [Key, << Type, Version:4/little-signed-integer-unit:8 >>]  ->
            {Key, -Version, Type};
        _ ->
            error(badkey)
    end.

lock_version(Version, Locks) ->
    Count = case lists:keyfind(Version, 1, Locks) of
                false -> 0;
                {Version, C} -> C
            end,
    lists:keyreplace(Version, 1, Locks, {Version, Count + 1}).

unlock_version(Version, Locks) ->
    case lists:keyfind(Version, 1, Locks) of
        false -> Locks;
        {Version, Count} ->
            Count2 = Count - 1,
            if
                Count2 >= 1 ->
                    lists:keyreplace(Version, 1, Locks, {Version, Count2});
                true ->
                    lists:keydelete(Version, 1, Locks)
            end
    end.
