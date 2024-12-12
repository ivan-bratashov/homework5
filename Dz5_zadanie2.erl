-module(Dz5_zadanie2).
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

-record(cache_entry, {key, value, expiry}).

create(TableName) ->
    ets:new(TableName, [named_table, public, set]).

insert(TableName, Key, Value) ->
    insert(TableName, Key, Value, undefined).

insert(TableName, Key, Value, TTL) ->
    Expiry = case TTL of
        undefined -> undefined;
        _ -> calendar:universal_time_to_seconds(calendar:universal_time()) + TTL
    end,
    ets:insert(TableName, #cache_entry{key = Key, value = Value, expiry = Expiry}).

lookup(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [#cache_entry{value = Value, expiry = undefined}] -> Value;
        [#cache_entry{value = Value, expiry = Expiry}] ->
            CurrentTime = calendar:universal_time_to_seconds(calendar:universal_time()),
            case Expiry > CurrentTime of
                true -> Value;
                false -> undefined
            end;
        _ -> undefined
    end.

delete_obsolete(TableName) ->
    CurrentTime = calendar:universal_time_to_seconds(calendar:universal_time()),
    ObsoleteKeys = [Key || #cache_entry{key = Key, expiry = Expiry} <- ets:tab2list(TableName),
                            Expiry =/= undefined, Expiry =< CurrentTime],
    [ets:delete(TableName, Key) || Key <- ObsoleteKeys].
