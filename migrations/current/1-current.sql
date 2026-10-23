
do $$
begin
  if not exists (
    select 1 from pg_attribute
    where attrelid = (select typrelid from pg_type where oid = 'public.competition_participation_record'::regtype)
      and attname = 'final_gain' and not attisdropped
  ) then
    alter type public.competition_participation_record add attribute final_gain numeric(10,3);
  end if;
end;
$$;

--!include functions/competition_reports.sql
--!include functions/activity_timeline.sql
