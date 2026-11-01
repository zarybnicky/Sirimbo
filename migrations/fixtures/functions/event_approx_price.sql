drop function if exists event_instance_approx_price;

create or replace function event_approx_price(v_instance event)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (v_instance.stats).total as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    where v_instance.type = 'lesson'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join event_trainer assignment on assignment.event_id = v_instance.id
  join tenant_trainer tt on tt.tenant_id = assignment.tenant_id
    and tt.person_id = assignment.person_id
    and tt.active_range @> v_instance.since
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_approx_price to anonymous;
comment on function event_approx_price is '@behavior +list -connection';
