
grant all on function immutable_concat_ws to anonymous;
grant all on function http_header to anonymous;
grant all on function http to trainer;
grant all on function fetch_with_cache to trainer;

--!include functions/tg_cohort_membership__on_status.sql
