# nakadi-cli

Command line interface for interacting with the [Nakadi event
broker](https://zalando.github.io/nakadi/) system developed by
[Zalando](https://github.com/zalando).

Example for doing a binary search against Nakadi cursors for locating
the cursor corresponding to some (`occurred_at`) timestamp:

```
$ export TOKEN="INSERT_AUTHENTICATION_TOKEN"
$ nakadi-cli --endpoint https://my-nakadi-endpoint find-cursors --event-type my-event-type-name --timestamp 2017-08-03T07:00:28.736Z
```
