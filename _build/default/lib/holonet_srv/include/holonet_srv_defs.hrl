-record(news, {
    id,
    title,
    content,
    timestamp
}).

-record(recent_news, {
    id,  % e.g. position or 'last30'
    news_ids = []
}).
