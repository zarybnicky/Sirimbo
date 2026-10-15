begin;

create schema if not exists tap;
create extension if not exists pgtap schema tap;
grant usage on schema tap to anonymous;
set local search_path to public, tap;
select tap.plan(5);

insert into federated.federation (code, name) values ('participation-test', 'Test');
insert into federated.category (id, name, series, discipline, age_group, class)
  overriding system value values (-380001, 'Test', 'participation-test', 'STT', '', '');
insert into federated.event (id, federation, external_id, start_date)
  overriding system value values (-380001, 'participation-test', 'event', '2026-01-01');
insert into federated.competition (id, federation, external_id, event_id, category_id, start_date)
  overriding system value
  values (-380001, 'participation-test', 'competition', -380001, -380001, '2026-01-01');
insert into federated.competitor (federation, external_id, competitor_type)
  select 'participation-test', id, 'solo' from unnest(array[
    'entry', 'cancelled', 'result', 'both', 'cancelled-result'
  ]) id;
insert into federated.competition_entry (competition_id, competitor_id, cancelled)
  values (-380001, 'participation-test:entry', false),
         (-380001, 'participation-test:cancelled', true),
         (-380001, 'participation-test:both', false),
         (-380001, 'participation-test:cancelled-result', true);
insert into federated.competition_result (competition_id, competitor_id, ranking)
  values (-380001, 'participation-test:result', 1),
         (-380001, 'participation-test:both', 2),
         (-380001, 'participation-test:cancelled-result', 3);

set local role anonymous;
select tap.results_eq(
  $$select competitor_id from federated.competition_participation
    where competition_id = -380001 order by competitor_id$$,
  $$values ('participation-test:both'::text), ('participation-test:cancelled-result'),
           ('participation-test:entry'), ('participation-test:result')$$,
  'public participation excludes cancelled entries and emits results exactly once'
);
select tap.is(
  (select count(*) from federated.competition_participation p
   join federated.competition_result r using (competition_id, competitor_id)
   where p.competition_id = -380001),
  3::bigint, 'results remain available through the composite-key relation'
);
select tap.ok(not has_table_privilege(current_user,
  'federated.competition_participation', 'insert'), 'anonymous cannot insert participation');
select tap.ok(not has_table_privilege(current_user,
  'federated.competition_participation', 'update'), 'anonymous cannot update participation');
select tap.ok(not has_table_privilege(current_user,
  'federated.competition_participation', 'delete'), 'anonymous cannot delete participation');
reset role;

select tap.finish(true);
rollback;
