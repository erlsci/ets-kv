-module(etskv_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    Store = etskv:open(test),
    etskv:put(<<"a">>, 1, Store),
    ?assertEqual(1, etskv:get(<<"a">>, Store)),
    etskv:put(<<"a">>, 2, Store),
    ?assertEqual(2, etskv:get(<<"a">>, Store)),
    etskv:put(<<"a1">>, 3, Store),
    ?assertEqual(3, etskv:get(<<"a1">>, Store)),
    ?assertEqual(2, etskv:get(<<"a">>, Store)),
    etskv:delete(<<"a">>, Store),
    ?assertError(not_found, etskv:get(<<"a">>, Store)),
    ?assertEqual(3, etskv:get(<<"a1">>, Store)),
    etskv:close(Store).

batch_test() ->
    Store = etskv:open(test),
    etskv:batch([{put, <<"a">>, 1},
                             {put, <<"b">>, 2},
                             {put, <<"c">>, 1}], Store),


    ?assert(etskv:contains(<<"a">>, Store)),
    ?assert(etskv:contains(<<"b">>, Store)),
    ?assert(etskv:contains(<<"c">>, Store)),
    ?assertEqual(1, etskv:get(<<"a">>, Store)),
    ?assertEqual(2, etskv:get(<<"b">>, Store)),
    ?assertEqual(1, etskv:get(<<"c">>, Store)),
    etskv:close(Store).

fold_keys_test() ->
        Store = etskv:open(test),
        ok =  etskv:batch([{put, <<"a">>, 1},
                           {put, <<"b">>, 2},
                           {put, <<"c">>, 3},
                           {put, <<"d">>, 4}], Store),

        AccFun = fun(K, Acc) -> [K | Acc] end,
        ?assertMatch([<<"a">>, <<"b">>, <<"c">>, <<"d">>],
                                 lists:reverse(etskv:fold_keys(AccFun, [], Store, []))),
        etskv:close(Store).

fold_gt_test() ->
        Store = etskv:open(test),
        ok =  etskv:batch([{put, <<"a">>, 1},
                           {put, <<"b">>, 2},
                           {put, <<"c">>, 3},
                           {put, <<"d">>, 4}], Store),

        AccFun = fun(K, V, Acc) ->
                         [{K, V} | Acc]
                 end,

        ?assertMatch([{<<"b">>, 2}, {<<"c">>, 3}, {<<"d">>, 4}],
                                 lists:reverse(etskv:fold(AccFun, [], Store,[{gt, <<"a">>}]))),
        etskv:close(Store).

fold_lt_test() ->
        Store = etskv:open(test),
        ok =  etskv:batch([{put, <<"a">>, 1},
                           {put, <<"b">>, 2},
                           {put, <<"c">>, 3},
                           {put, <<"d">>, 4}], Store),

        AccFun = fun(K, V, Acc) ->
                     [{K, V} | Acc]
                 end,

        ?assertMatch([{<<"a">>, 1}, {<<"b">>, 2}, {<<"c">>, 3}],
                                 lists:reverse(etskv:fold(AccFun, [], Store, [{lt, <<"d">>}]))),
        etskv:close(Store).

fold_lt_gt_test() ->
        Store = etskv:open(test),
        ok =  etskv:batch([{put, <<"a">>, 1},
                           {put, <<"b">>, 2},
                           {put, <<"c">>, 3},
                           {put, <<"d">>, 4}], Store),

        AccFun = fun(K, V, Acc) ->
                      [{K, V} | Acc]
                 end,

        ?assertMatch([{<<"b">>, 2}, {<<"c">>, 3}],
                                 lists:reverse(etskv:fold(
                                                                 AccFun, [], Store,
                                                                 [{gt, <<"a">>},  {lt, <<"d">>}]))),
        etskv:close(Store).

fold_lt_gt_max_test() ->
        Store = etskv:open(test),
        ok =  etskv:batch([{put, <<"a">>, 1},
                           {put, <<"b">>, 2},
                           {put, <<"c">>, 3},
                           {put, <<"d">>, 4}], Store),



        AccFun = fun(K, V, Acc) ->
                         [{K, V} | Acc]
                 end,

        ?assertMatch([{<<"b">>, 2}],
                                 etskv:fold(AccFun, [], Store,  [{gt, <<"a">>},
                                                                 {lt, <<"d">>},
                                                                 {max, 1}])),
        etskv:close(Store).
