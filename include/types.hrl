-type store() :: #{tab => atom(), writer => pid()}.
-type iterator() :: pid().

-type key() :: binary().
-type value() :: term() | any().
-type batch_ops() :: [{put, key(), value()} | {delete, key()} | {del, key()}].
-type fold_options() :: [{start_key, key()}
                         | {end_key, key()}
                         | {gt, key()}
                         | {gte, key()}
                         | {lt, key()}
                         | {lte, key()}
                         | {max, non_neg_integer()}].

-type iterator_ops() :: first | last | next | prev | binary().
-type iterator_options() :: [keys_only].
