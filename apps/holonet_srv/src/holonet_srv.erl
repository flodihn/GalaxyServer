-module(holonet_srv).

-behaviour(gen_server).

-include("holonet_srv_defs.hrl").

-export([start_link/0]).

-export([add_news/2, get_recent_news/0, remove_news/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DB_NEWS_TABLE, holonet_news).
-define(DB_RECENT_TABLE, holonet_recent).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_news(Title, Content) when is_binary(Title), is_binary(Content) ->
    gen_server:call(?MODULE, {add_news, Title, Content}).

get_recent_news() ->
    gen_server:call(?MODULE, get_recent_news).

remove_news(Id) when is_integer(Id) ->
    gen_server:call(?MODULE, {remove_news, Id});

remove_news(all) ->
    gen_server:call(?MODULE, {remove_news, all}).

init([]) ->
    case init_mnesia() of
        ok ->
            {ok, #{}};
        {error, Reason} ->
            {stop, Reason}
    end.

init_mnesia() ->
    mnesia:start(),
    case ensure_news_table() of
        ok ->
            case ensure_recent_table() of
                ok ->
                    case wait_for_tables([?DB_NEWS_TABLE, ?DB_RECENT_TABLE]) of
                        ok ->
                            ensure_recent_record();
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

ensure_news_table() ->
    ensure_table(
        ?DB_NEWS_TABLE,
        news,
        record_info(fields, news)).

ensure_recent_table() ->
    ensure_table(
        ?DB_RECENT_TABLE,
        recent_news,
        record_info(fields, recent_news)).

ensure_table(Table, RecordName, Attrs) ->
    change_to_disc_schema(),
    case mnesia:create_table(Table, [
        {record_name, RecordName},
        {disc_copies, [node()]},
        {type, set},
        {attributes, Attrs}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}} ->
            ok;
        {aborted, {already_exists, Table, _}} ->
            ok;
        {aborted, Reason} ->
            {error, {create_table_failed, Table, Reason}}
    end.

wait_for_tables(Tables) ->
    wait_for_tables(Tables, 10).

wait_for_tables(_Tables, 0) ->
    {error, mnesia_tables_not_ready};
wait_for_tables(Tables, Attempts) ->
    case mnesia:wait_for_tables(Tables, 30000) of
        ok ->
            ok;
        {timeout, BadTables} ->
            logger:warning(
                "holonet_srv: tables not ready ~p (~p attempts left), retrying",
                [BadTables, Attempts - 1]),
            timer:sleep(200),
            wait_for_tables(BadTables, Attempts - 1)
    end.

ensure_recent_record() ->
    ensure_recent_record(10).

ensure_recent_record(0) ->
    {error, ensure_recent_record_failed};
ensure_recent_record(Attempts) ->
    case mnesia:transaction(fun() ->
        case mnesia:read(?DB_RECENT_TABLE, last30) of
            [] ->
                mnesia:write(
                    ?DB_RECENT_TABLE,
                    #recent_news{id = last30, news_ids = []},
                    write);
            _ ->
                ok
        end
    end) of
        {atomic, _} ->
            ok;
        {aborted, {no_exists, _}} ->
            wait_for_tables([?DB_RECENT_TABLE], 3),
            ensure_recent_record(Attempts - 1);
        {aborted, Reason} ->
            logger:error("holonet_srv: ensure_recent_record aborted: ~p", [Reason]),
            {error, {ensure_recent_record_failed, Reason}}
    end.

read_recent_news() ->
    case mnesia:read(?DB_RECENT_TABLE, last30) of
        [Recent] ->
            Recent;
        [] ->
            #recent_news{id = last30, news_ids = []}
    end.

change_to_disc_schema() ->
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

handle_call({add_news, Title, Content}, _From, State) ->
    Timestamp = erlang:system_time(seconds),
    NewsId = erlang:unique_integer([positive]),
    News = #news{
        id = NewsId,
        title = Title,
        content = Content,
        timestamp = Timestamp
    },
    Result = mnesia:transaction(fun() ->
        mnesia:write(?DB_NEWS_TABLE, News, write),
        Recent = read_recent_news(),
        NewIds = [NewsId | Recent#recent_news.news_ids],
        Trimmed = lists:sublist(NewIds, 30),
        mnesia:write(?DB_RECENT_TABLE, Recent#recent_news{news_ids = Trimmed}, write),
        {ok, NewsId}
    end),
    {reply, Result, State};

handle_call(get_recent_news, _From, State) ->
    Result = mnesia:transaction(fun() ->
        Recent = read_recent_news(),
        NewsList = lists:foldl(fun(Id, Acc) ->
            case mnesia:read(?DB_NEWS_TABLE, Id) of
                [News] -> [News | Acc];
                [] -> Acc
            end
        end, [], Recent#recent_news.news_ids),
        lists:reverse(NewsList)  % most recent first
    end),
    {reply, Result, State};

handle_call({remove_news, Id}, _From, State) when is_integer(Id) ->
    Result = mnesia:transaction(fun() ->
        case mnesia:read(?DB_NEWS_TABLE, Id) of
            [_News] ->
                mnesia:delete({?DB_NEWS_TABLE, Id}),
                Recent = read_recent_news(),
                NewIds = lists:delete(Id, Recent#recent_news.news_ids),
                mnesia:write(
                    ?DB_RECENT_TABLE,
                    Recent#recent_news{news_ids = NewIds},
                    write),
                ok;
            [] ->
                {error, not_found}
        end
    end),
    {reply, Result, State};

handle_call({remove_news, all}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        lists:foreach(
            fun(Key) -> mnesia:delete({?DB_NEWS_TABLE, Key}) end,
            mnesia:all_keys(?DB_NEWS_TABLE)),
        mnesia:write(
            ?DB_RECENT_TABLE,
            #recent_news{id = last30, news_ids = []},
            write),
        ok
    end),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
