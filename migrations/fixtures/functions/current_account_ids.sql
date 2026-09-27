create or replace function current_account_ids() returns setof bigint
  language sql stable
as $$
  select id from account
  where person_id = any ((select current_person_ids())::bigint[]);
$$;

comment on function current_account_ids() is '@omit';
grant execute on function current_account_ids() to anonymous;
