-module(holonet_srv).

-behaviour(gen_server).

-include("holonet_srv_defs.hrl").

-export([start_link/0]).

-export([submit_news/2, get_recent_news/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DB_NEWS_TABLE, holonet_news).
-define(DB_RECENT_TABLE, holonet_recent).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit_news(Title, Content) when is_binary(Title), is_binary(Content) ->
    gen_server:call(?MODULE, {submit_news, Title, Content}).

get_recent_news() ->
    gen_server:call(?MODULE, get_recent_news).

init([]) ->
    init_mnesia(),
    {ok, #{}}.

init_mnesia() ->
    mnesia:start(),
    create_news_table(),
    create_recent_table().

create_news_table() ->
    case lists:member(?DB_NEWS_TABLE, mnesia:system_info(tables)) of
        true -> ok;
        false ->
            change_to_disc_schema(),
            mnesia:create_table(?DB_NEWS_TABLE, [
                {record_name, news},
                {disc_copies, [node()]},
                {type, set},
                {attributes, record_info(fields, news)}
            ]),
            ok
    end.

create_recent_table() ->
    case lists:member(?DB_RECENT_TABLE, mnesia:system_info(tables)) of
        true -> ok;
        false ->
            change_to_disc_schema(),
            mnesia:create_table(?DB_RECENT_TABLE, [
                {record_name, recent_news},
                {disc_copies, [node()]},
                {type, set},
                {attributes, record_info(fields, recent_news)}
            ]),
            ok
    end,
    ensure_recent_record().

ensure_recent_record() ->
    {atomic, _} = mnesia:transaction(fun() ->
        case mnesia:read(?DB_RECENT_TABLE, last30) of
            [] ->
                mnesia:write(?DB_RECENT_TABLE, #recent_news{id = last30, news_ids = []}, write);
            _ -> ok
        end
    end),
    ok.

change_to_disc_schema() ->
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

handle_call({submit_news, Title, Content}, _From, State) ->
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
        % Update recent
        [Recent] = mnesia:read(?DB_RECENT_TABLE, last30),
        NewIds = [NewsId | Recent#recent_news.news_ids],
        Trimmed = lists:sublist(NewIds, 30),
        mnesia:write(?DB_RECENT_TABLE, Recent#recent_news{news_ids = Trimmed}, write),
        {ok, NewsId}
    end),
    {reply, Result, State};

handle_call(get_recent_news, _From, State) ->
    Result = mnesia:transaction(fun() ->
        [Recent] = mnesia:read(?DB_RECENT_TABLE, last30),
        NewsList = lists:foldl(fun(Id, Acc) ->
            case mnesia:read(?DB_NEWS_TABLE, Id) of
                [News] -> [News | Acc];
                [] -> Acc
            end
        end, [], Recent#recent_news.news_ids),
        lists:reverse(NewsList)  % most recent first
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
