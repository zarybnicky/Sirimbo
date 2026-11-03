CREATE FUNCTION public.event_approx_price(v_instance public.event) RETURNS TABLE(amount numeric, currency text)
    LANGUAGE sql STABLE
    AS $$
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

COMMENT ON FUNCTION public.event_approx_price(v_instance public.event) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.event_approx_price(v_instance public.event) TO anonymous;
