begin;
set local search_path to public, tap;
select tap.plan(13);

insert into federated.federation (code, name) values ('wdsf', 'WDSF'), ('csts', 'ČSTS')
on conflict (code) do nothing;
insert into federated.category (id, name, series, discipline, age_group, class)
overriding system value values (-430001, 'Test', 'round-dances-test', 'LATIN', '', '');
insert into federated.event (id, federation, external_id, start_date)
overriding system value values (-430001, 'wdsf', 'round-dances-test', '2026-01-01'),
  (-430002, 'csts', 'round-dances-test', '2026-01-01');
insert into federated.competition (id, federation, external_id, event_id, category_id, start_date)
overriding system value values (-430001, 'wdsf', 'round-dances-test', -430001, -430001, '2026-01-01'),
  (-430002, 'csts', 'round-dances-test', -430002, -430001, '2026-01-01');
insert into federated.dance_program (code, name)
values ('CH-JI-PD-RU-SA', 'CH JI PD RU SA'), ('PK-SW-CH', 'PK SW CH') on conflict (code) do nothing;
insert into federated.dance_program_dance (program_id, dance_code, dance_order)
select p.id, d.code, d.position::integer from federated.dance_program p
cross join lateral unnest(string_to_array(p.code, '-')) with ordinality as d(code, position)
where p.code in ('CH-JI-PD-RU-SA', 'PK-SW-CH') on conflict do nothing;
insert into federated.competition_round (id, competition_id, round_key, round_index, dance_program_id, scoring_method)
overriding system value
select r.id, r.competition_id, r.key, r.index, p.id, r.method::federated.scoring_method
from (values
  (-430001, -430001, '1', 1, 'CH-JI-PD-RU-SA', 'skating_marks'),
  (-430002, -430001, 'F', 2, 'CH-JI-PD-RU-SA', 'ajs-3.0'),
  (-430003, -430001, 'empty', 3, 'CH-JI-PD-RU-SA', 'skating_marks'),
  (-430004, -430002, '1', 1, 'PK-SW-CH', 'skating_marks')
) r(id, competition_id, key, index, program, method)
join federated.dance_program p on p.code = r.program;
insert into federated.round_dance (round_id, dance_program_id, dance_code, dance_order)
select r.id, r.dance_program_id, d.dance_code, d.dance_order
from federated.competition_round r
join federated.dance_program_dance d on d.program_id = r.dance_program_id
where r.competition_id in (-430001, -430002);
insert into federated.judge_score (round_id, dance_order, dance_code, judge_person_id, competitor_id, component, score, raw_score)
values (-430001, 2, 'JI', 1, 'partial', 'mark', 1, 'X'),
  (-430001, 3, 'PD', 1, 'partial', 'mark', 0, null),
  (-430001, 1, 'CH', 1, 'full', 'mark', 1, ''),
  (-430001, 2, 'JI', 1, 'full', 'mark', 1, null),
  (-430001, 3, 'PD', 1, 'full', 'mark', 1, null),
  (-430001, 4, 'RU', 1, 'full', 'mark', 1, null),
  (-430001, 5, 'SA', 1, 'full', 'mark', 1, null),
  (-430002, 1, 'CH', 1, 'full', 'ajs_tq', 8.125, '8.125'),
  (-430002, 4, 'RU', 1, 'full', 'ajs_tq', 9.375, null),
  (-430002, 2, 'JI', 1, 'full', 'ajs_tq', 0, null),
  (-430002, 2, 'JI', 1, 'full', 'ajs_mm', 0, null),
  (-430002, 2, 'JI', 1, 'full', 'ajs_reduction', 0, null);
create temporary table before_scores as
select s.*, s.ctid as physical_position from federated.judge_score s where round_id in (-430001, -430002);

select tap.is(app_private.refresh_wdsf_round_dances(-430001), 3, 'updates only the three WDSF rounds');
select tap.is((select array_agg(dance_code order by dance_order) from federated.round_dance where round_id = -430001),
  array['SA','CH','RU','PD','JI'], 'collects dances across all participants and orders Latin dances');
select tap.is((select array_agg(dance_code order by dance_order) from federated.round_dance where round_id = -430002),
  array['CH','RU','JI'], 'removes scoreless dances and retains zero-valued score records');
select tap.is((select p.code from federated.competition_round r join federated.dance_program p on p.id = r.dance_program_id
  where r.id = -430003), '', 'retains the scoreless round with an empty program');
select tap.results_eq(
  $$select round_id, dance_code, judge_person_id, competitor_id, component, score, raw_score
    from federated.judge_score where round_id in (-430001, -430002)
    order by round_id, dance_code, judge_person_id, competitor_id, component$$,
  $$select round_id, dance_code, judge_person_id, competitor_id, component, score, raw_score
    from before_scores order by round_id, dance_code, judge_person_id, competitor_id, component$$,
  'preserves all scores, including missing participant dances, zero values, precision, and raw text');
select tap.ok((select s.ctid = b.physical_position from federated.judge_score s join before_scores b
  using (round_id, dance_order, judge_person_id, competitor_id, component)
  where s.round_id = -430002 and s.dance_code = 'CH'), 'does not rewrite a correctly placed score');
create temporary table after_scores as
select s.*, s.ctid as physical_position from federated.judge_score s where round_id in (-430001, -430002);
select tap.is(app_private.refresh_wdsf_round_dances(-430001), 0, 'another pass changes no rounds');
select tap.ok(not exists (
  select 1 from federated.judge_score s join after_scores a
    using (round_id, dance_order, judge_person_id, competitor_id, component)
  where s.ctid <> a.physical_position
), 'another pass writes no score rows');
select tap.is(app_private.refresh_wdsf_round_dances(-430002), 0, 'does not process ČSTS');
select tap.is((select array_agg(dance_code order by dance_order) from federated.round_dance where round_id = -430004),
  array['PK','SW','CH'], 'preserves ČSTS source order and scoreless dances');

insert into federated.dance_program (code, name) values ('CH-RU-JI-PD-SA', 'CH RU JI PD SA') on conflict do nothing;
insert into federated.dance_program_dance (program_id, dance_code, dance_order)
select p.id, d.code, d.position::integer from federated.dance_program p
cross join lateral unnest(string_to_array(p.code, '-')) with ordinality as d(code, position)
where p.code = 'CH-RU-JI-PD-SA' on conflict do nothing;
update federated.competition_round set dance_program_id = (select id from federated.dance_program where code = 'CH-RU-JI-PD-SA')
where id = -430002;
insert into federated.round_dance (round_id, dance_program_id, dance_code, dance_order)
select -430002, dance_program_id, d.code, d.position from federated.competition_round
cross join (values ('PD', 4), ('SA', 5)) d(code, position) where id = -430002;
insert into federated.judge_score (round_id, dance_order, dance_code, judge_person_id, competitor_id, component, score)
values (-430002, 4, 'PD', 2, 'later', 'ajs_tq', 7.5), (-430002, 5, 'SA', 2, 'later', 'ajs_tq', 8.5);
select tap.is(app_private.refresh_wdsf_round_dances(-430001), 1, 'a later participant expands only its affected round');
select tap.is((select p.code from federated.competition_round r join federated.dance_program p on p.id = r.dance_program_id
  where r.id = -430002), 'SA-CH-RU-PD-JI', 'expands the observed set without assuming a fixed program');
select tap.is((select score from federated.judge_score where round_id = -430002 and dance_code = 'CH'),
  8.125::numeric, 'preserves earlier participant scores after expansion');

select tap.finish(true);
rollback;
