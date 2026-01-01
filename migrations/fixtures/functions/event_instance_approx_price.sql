create or replace function public.event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (select count(*)
       from event e
       join lateral event_registrants(e.*) r on true
       where e.id = v_instance.event_id)::bigint as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
  )
  select
    coalesce(sum((tt.member_price_45min).amount * s.duration / 45 / nullif(s.num_participants, 0)), 'NaN') as amount,
    coalesce((tt.member_price_45min).currency, 'CZK') as currency
  from stats s
  join lateral public.event_instance_trainers(v_instance) tt on true
  group by (tt.member_price_45min).currency;
$$;

grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';
