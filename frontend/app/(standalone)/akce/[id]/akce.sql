/* @name EventSeriesEvents */
select i.id from event_series s join event i on s.id = i.series_id where s.id = :id;
