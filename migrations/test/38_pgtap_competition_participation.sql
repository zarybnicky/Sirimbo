begin;

set local search_path to public, tap;
select tap.plan(7);

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
insert into federated.competition_result (competition_id, competitor_id, ranking, start_number)
  values (-380001, 'participation-test:result', 1, '101'),
         (-380001, 'participation-test:both', 2, '102'),
         (-380001, 'participation-test:cancelled-result', 3, null);

set local role anonymous;
select tap.results_eq(
  $$select competitor_id from federated.competition_participation
    where competition_id = -380001 order by competitor_id$$,
  $$values ('participation-test:both'::text), ('participation-test:cancelled'),
           ('participation-test:cancelled-result'),
           ('participation-test:entry'), ('participation-test:result')$$,
  'public participation includes cancelled entries and emits results exactly once'
);
select tap.is(
  (select count(*) from federated.competition_participation p
   join federated.competition_result r using (competition_id, competitor_id)
   where p.competition_id = -380001),
  3::bigint, 'results remain available through the composite-key relation'
);
select tap.ok(not has_table_privilege(current_user,
  'federated.competition_participation', 'insert,update,delete'), 'anonymous cannot write participation');
select tap.results_eq(
  $$select cancelled, ranking from federated.competition_participation
    where competition_id = -380001 and competitor_id = 'participation-test:cancelled-result'$$,
  $$values (true, 3)$$, 'participation retains cancellation together with result fields'
);
select tap.results_eq(
  $$select competitor_id, start_number from federated.competition_participation
    where competition_id = -380001 order by competitor_id$$,
  $$values ('participation-test:both'::text, '102'::text),
           ('participation-test:cancelled', null),
           ('participation-test:cancelled-result', null),
           ('participation-test:entry', null), ('participation-test:result', '101')$$,
  'participation exposes result start numbers and leaves unnumbered entries empty'
);
reset role;

insert into public.person (id, first_name, last_name, gender, nationality, csts_id)
  overriding system value values (-380001, 'Calendar', 'Official', 'unspecified', '', -380001);
insert into federated.person (federation, external_id) values ('csts', -380001);
insert into federated.competition (id, federation, external_id, event_id, category_id, start_date)
  overriding system value values
  (-380002, 'participation-test', 'second-day', -380001, -380001, '2026-01-02');
insert into federated.event_official (event_id, person_id, role)
  values (-380001, 'csts:-380001', 'adjudicator');
insert into federated.competition_official (competition_id, person_id, role)
  values (-380001, 'csts:-380001', 'adjudicator'), (-380002, 'csts:-380001', 'adjudicator');
select tap.is(
  (select count(*) from federated.official_assignment
    where event_id = -380001 and date = '2026-01-01'),
  1::bigint, 'official assignments deduplicate event and competition roles on the same day'
);
select tap.is(
  (select date from federated.official_assignment
    where event_id = -380001 and date > '2026-01-01'),
  '2026-01-02'::date, 'competition officials retain their competition date'
);

select tap.finish(true);
rollback;
