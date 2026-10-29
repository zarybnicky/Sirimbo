create or replace function app_private.refresh_wdsf_round_dances(p_competition_id bigint)
returns integer
language plpgsql
as $$
declare
  preferred_order text[];
  r record;
  dance_codes text[];
  old_orders integer[];
  program_codes text[];
  target_program_code text;
  target_program_id bigint;
  changed_rounds integer := 0;
begin
  select case
    when cat.discipline in ('SMOOTH', 'PD SMOOTH') then array['SW', 'TG', 'SF', 'VW']
    when cat.discipline = 'RHYTHM' then array['CH', 'RU', 'SG', 'BO', 'MA']
    else array['SW', 'TG', 'VW', 'SF', 'QS', 'SA', 'CH', 'RU', 'PD', 'JI']
  end into preferred_order
  from federated.competition c join federated.category cat on cat.id = c.category_id
  where c.id = p_competition_id and c.federation = 'wdsf'
  for no key update of c;
  if not found then return 0; end if;

  for r in
    with dances as (
      select cr.id, dp.code as program_code, rd.dance_code, rd.dance_order,
        exists (
          select 1 from federated.judge_score js
          where (js.round_id, js.dance_order, js.dance_code)
            = (rd.round_id, rd.dance_order, rd.dance_code)
        ) as has_scores
      from federated.competition_round cr
      join federated.dance_program dp on dp.id = cr.dance_program_id
      left join federated.round_dance rd on rd.round_id = cr.id
      where cr.competition_id = p_competition_id
    )
    select id, dances.program_code,
      coalesce(array_agg(dance_code order by dance_order)
        filter (where dance_code is not null), '{}') as current_codes,
      coalesce(array_agg(dance_order order by dance_order)
        filter (where dance_order is not null), '{}') as current_orders,
      coalesce(array_agg(dance_code order by dance_order)
        filter (where has_scores), '{}') as scored_codes,
      coalesce(array_agg(dance_order order by dance_order)
        filter (where has_scores), '{}') as scored_orders
    from dances
    group by id, dances.program_code
    order by id
  loop
    dance_codes := r.scored_codes;
    old_orders := r.scored_orders;
    if dance_codes <@ preferred_order then
      select coalesce(array_agg(code order by array_position(preferred_order, code), old_order), '{}'),
        coalesce(array_agg(old_order order by array_position(preferred_order, code), old_order), '{}')
      into dance_codes, old_orders
      from unnest(dance_codes, old_orders) as d(code, old_order);
    end if;

    select coalesce(array_agg(code order by position), '{}') into program_codes
    from (
      select code, min(position) as position
      from unnest(dance_codes) with ordinality as d(code, position)
      group by code
    ) as unique_dances;
    target_program_code := array_to_string(program_codes, '-');
    if r.program_code is not distinct from target_program_code
      and r.current_codes = dance_codes
      and r.current_orders = array(select generate_series(1, cardinality(dance_codes)))
    then
      continue;
    end if;

    insert into federated.dance_program (code, name)
    values (target_program_code, array_to_string(program_codes, ' '))
    on conflict (code) do nothing
    returning id into target_program_id;
    if target_program_id is null then
      select id into strict target_program_id from federated.dance_program where code = target_program_code;
    end if;
    insert into federated.dance_program_dance (program_id, dance_code, dance_order)
    select target_program_id, code, position::integer
    from unnest(program_codes) with ordinality as d(code, position)
    on conflict (program_id, dance_code) do nothing;

    delete from federated.round_dance rd
    where rd.round_id = r.id
      and not exists (select 1 from federated.judge_score js
                      where (js.round_id, js.dance_order) = (rd.round_id, rd.dance_order));
    update federated.competition_round set dance_program_id = target_program_id
    where id = r.id and dance_program_id <> target_program_id;

    if old_orders <> array(select generate_series(1, cardinality(dance_codes))) then
      -- Swap dance positions without key collisions. Check score references
      -- after both the dance positions and their scores agree again.
      set constraints federated.judge_score_round_id_dance_order_dance_code_fkey deferred;
      update federated.round_dance rd set dance_order = -rd.dance_order
      from unnest(old_orders) with ordinality as d(old_order, position)
      where rd.round_id = r.id and rd.dance_order = d.old_order and d.old_order <> d.position;
      update federated.round_dance rd set dance_order = d.position::integer
      from unnest(old_orders) with ordinality as d(old_order, position)
      where rd.round_id = r.id and rd.dance_order = -d.old_order and d.old_order <> d.position;

      with moved as materialized (
        select js.round_id, d.position::integer as dance_order, js.dance_code,
          js.judge_person_id, js.competitor_id, js.component, js.score, js.raw_score
        from federated.judge_score js
        join unnest(old_orders) with ordinality as d(old_order, position) on d.old_order = js.dance_order
        where js.round_id = r.id and d.old_order <> d.position
      ), removed as (
        delete from federated.judge_score js
        where js.round_id = r.id
          and js.dance_order in (select old_order from unnest(old_orders) with ordinality as d(old_order, position)
                                where old_order <> position)
          and not exists (select 1 from moved m where
            (m.dance_order, m.judge_person_id, m.competitor_id, m.component)
            = (js.dance_order, js.judge_person_id, js.competitor_id, js.component))
      )
      insert into federated.judge_score as target (
        round_id, dance_order, dance_code, judge_person_id, competitor_id, component, score, raw_score
      )
      select * from moved
      on conflict (round_id, dance_order, judge_person_id, competitor_id, component) do update
      set dance_code = excluded.dance_code, score = excluded.score, raw_score = excluded.raw_score
      where (target.dance_code, target.score, target.raw_score)
        is distinct from (excluded.dance_code, excluded.score, excluded.raw_score);
      set constraints federated.judge_score_round_id_dance_order_dance_code_fkey immediate;
    end if;
    changed_rounds := changed_rounds + 1;
  end loop;
  return changed_rounds;
end;
$$;
