-module(etskv_iter_tests).

-include_lib("eunit/include/eunit.hrl").

move_test() ->
    Store = etskv:open(test),
    etskv:batch([{put, <<"a">>, 1},
                 {put, <<"b">>, 2},
                 {put, <<"c">>, 1}], Store),

    Iterator = etskv_iter:new(Store),
    etskv:put(<<"a">>, 2, Store),
    ?assertEqual(2, etskv:get(<<"a">>, Store)),
    ?assertEqual({ok, <<"a">>, 1}, etskv_iter:move(Iterator, next)),
    ?assertEqual({ok, <<"b">>, 2}, etskv_iter:move(Iterator, next)),
    ?assertEqual({ok, <<"c">>, 1}, etskv_iter:move(Iterator, next)),
    ?assertEqual('$iterator_limit', etskv_iter:move(Iterator, next)),
    ?assertEqual('$iterator_limit', etskv_iter:move(Iterator, next)),
    ?assertEqual({ok, <<"c">>, 1}, etskv_iter:move(Iterator, prev)),
    ?assertEqual({ok, <<"b">>, 2}, etskv_iter:move(Iterator, prev)),
    ?assertEqual({ok, <<"a">>, 1}, etskv_iter:move(Iterator, prev)),
    ?assertEqual('$iterator_limit', etskv_iter:move(Iterator, prev)),
    ?assertEqual('$iterator_limit', etskv_iter:move(Iterator, prev)),
    ?assertEqual({ok, <<"a">>, 1}, etskv_iter:move(Iterator, next)),
    ?assertEqual({ok, <<"a">>, 1}, etskv_iter:move(Iterator, first)),
    ?assertEqual({ok, <<"c">>, 1}, etskv_iter:move(Iterator, last)),
    ?assertEqual({ok, <<"b">>, 2}, etskv_iter:move(Iterator, <<"b">>)),
    ?assertEqual({ok, <<"c">>, 1}, etskv_iter:move(Iterator, next)),
    etskv_iter:close(Iterator),
    etskv:close(Store).

close_test() ->
    Store = etskv:open(test),
    Iterator = etskv_iter:new(Store),
    etskv_iter:close(Iterator),
    ?assertEqual({error, iterator_closed}, etskv_iter:move(Iterator, next)),
    etskv:close(Store).

multiple_iterators_test() ->
    Store = etskv:open(test),
    etskv:batch([{put, <<"a">>, 3},
                 {put, <<"b">>, 1},
                 {put, <<"c">>, 2}], Store),
    Iterator = etskv_iter:new(Store),
    etskv_iter:close(Iterator),

    Iterator2 = etskv_iter:new(Store),
    ?assertEqual({ok, <<"a">>, 3}, etskv_iter:move(Iterator2, next)),
    ?assertEqual({ok, <<"b">>, 1}, etskv_iter:move(Iterator2, next)),
    ?assertEqual({ok, <<"c">>, 2}, etskv_iter:move(Iterator2, next)),
    ?assertEqual('$iterator_limit', etskv_iter:move(Iterator2, next)),
    etskv_iter:close(Iterator2),
    etskv:close(Store).
