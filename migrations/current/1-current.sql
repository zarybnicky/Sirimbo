
drop function if exists app_private.index_advisor;
drop function if exists app_private.calculate_transaction_effective_date;

--!include functions/tg_transaction__effective_date.sql
--!include functions/event_overlaps_reports.sql
