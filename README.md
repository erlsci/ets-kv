# memdb

[![Build Status][gh-actions-badge]][gh-actions]
[![Erlang Versions][erlang-badge]][versions]
[![Tags][github-tags-badge]][github-tags]

*A simple in-memory K/V store with MVCC semantics*

[![][logo]][logo-large]

##### Contents

* [Introduction](#introduction-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Usage](#usage-)
* [License](#license-)

## Overview [&#x219F;](#contents)

memdb is K/V store built on top of [ETS](http://www.erlang.org/doc/man/ets.html). memdb compared to ETS provides
concurrent access to the database using [MVCC](https://en.wikipedia.org/wiki/Multiversion_concurrency_control) allowing
multiple reader to read concurrently a consitent view of the database without locking.

All writes are serialized for now.

A MemDB's memory consumption increases monotonically, even if keys are deleted or values are updated. Compaction can be
triggered manually or automatically using the auto-vacuum. A full snapshot can be stored or copied in another database
if needed.

## Documentation [&#x219F;](#contents)

Full doc is available in [`memdb`](doc/memdb.md).

## Build [&#x219F;](#contents)

```shell
$ rebar3 compile
```

## Usage


Start up an Erlang shell:

``` shell
$ rebar3 shell
```

### Create a database [&#x219F;](#contents)

```erl
Name = mydb.
Db = memdb:open(Name).
```

### Store a values [&#x219F;](#contents)

Storing a value associated to a key using `memdb:put/3`:

```erl
Key = <<"a">>,
Value = 1,
ok =  memdb:put(Key, Value, Db).
```

### Retrieve a value [&#x219F;](#contents)

Use the `memdb:get/2` function to retrieve a value.

```erl
Value = memdb:get(Key, Db).
```

Value should be `1`. Note that you can use `memdb:contains/2` to check ahead of time:

``` erl
memdb:contains(Key, Db).
```

### Delete a value [&#x219F;](#contents)

Use `memdb:delete/2` to delete a value:

```erl
ok = memdb:delete(Key, Db).
```

### Working with Multiple Values [&#x219F;](#contents)

#### Storing

Using `memdb:write_batch/2` you can write and delete multiple values in one
pass:

```erl
ok =  memdb:write_batch([{put, <<"a">>, 1},
                         {put, <<"b">>, 2},
                         {put, <<"c">>, 3}], Db),

ok =  memdb:write_batch([{put, <<"d">>, 4},
                         {delete, <<"b">>},
                         {put, <<"e">>, 5}], Db).
```

#### Retrieving

Use `memdb:fold/4` to retrieve multiples K/Vs

### Close Db [&#x219F;](#contents)

Close a storage using `memdb:close/1`:

```erl
memdb:close(Db)
```

## License [&#x219F;](#contents)

```
Copyright © 2015 Benoît Chesneau
Copyright © 2024 Duncan McGreggor

Distributed under the Mozilla Public License Version 2.0.
```

[//]: ---Named-Links---

[logo]: priv/images/logo.png
[logo-large]: priv/images/logo-large.png
[gh-actions-badge]: https://github.com/erlsci/memdb/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/erlsci/memdb/actions
[erlang-badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/erlsci/memdb/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/erlsci/memdb/tags
[github-tags-badge]: https://img.shields.io/github/tag/erlsci/memdb.svg
