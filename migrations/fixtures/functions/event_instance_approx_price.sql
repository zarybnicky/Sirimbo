create or replace function event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as materialized (
    select
      count(*) as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    from event_instance_registration registration
    where
      v_instance.type = 'lesson'
      and registration.instance_id = v_instance.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral event_instance_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_instance_approx_price to anonymous;
comment on function event_instance_approx_price is '@simpleCollections only';
