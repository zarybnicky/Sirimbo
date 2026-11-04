begin;
set local search_path = public, tap;
set local timezone = 'UTC';
select tap.no_plan();

insert into crawler.refetch_policy (federation, kind, reference_offset, fetch_interval) values
  ('refetch-test', 'event', null, '1 month'),
  ('refetch-test', 'event', '-1 month', '1 week'),
  ('refetch-test', 'event', '-1 week', '1 day'),
  ('refetch-test', 'event', '1 month', '1 month'),
  ('refetch-test', 'event', '1 year', '1 year');

select tap.is(crawler.next_fetch_at('refetch-test', 'event', null, '2026-06-01 12:00Z'),
  '2026-07-01 12:00Z'::timestamptz, 'unknown date uses the default');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-12-01', '2026-06-01 12:00Z'),
  '2026-07-01 12:00Z'::timestamptz, 'distant future uses the default');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-06-20', '2026-06-01 12:00Z'),
  '2026-06-08 12:00Z'::timestamptz, 'approaching event uses a weekly interval');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-06-05', '2026-06-01 12:00Z'),
  '2026-06-02 12:00Z'::timestamptz, 'imminent event uses a daily interval');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-05-31', '2026-06-01 12:00Z'),
  '2026-06-02 12:00Z'::timestamptz, 'recent past event uses a daily interval');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-04-01', '2026-06-01 12:00Z'),
  '2026-07-01 12:00Z'::timestamptz, 'older event uses a monthly interval');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2020-01-01', '2026-06-01 12:00Z'),
  '2027-06-01 12:00Z'::timestamptz, 'historical event uses an annual interval');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-07-02', '2026-06-01 12:00Z'),
  '2026-06-02 00:00Z'::timestamptz, 'long interval cannot skip the next rule');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-06-08', '2026-06-01 00:00Z'),
  '2026-06-02 00:00Z'::timestamptz, 'new interval applies exactly at the boundary');
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-01-31', '2026-02-28 12:00Z'),
  '2026-03-28 12:00Z'::timestamptz, 'month offsets use calendar arithmetic');
set local timezone = 'Pacific/Auckland';
select tap.is(crawler.next_fetch_at('refetch-test', 'event', '2026-06-08', '2026-06-01 00:00Z'),
  '2026-06-02 00:00Z'::timestamptz, 'session timezone does not move rule boundaries');
set local timezone = 'UTC';

insert into crawler.frontier (federation,kind,key,fetch_status,process_status,last_fetched_at,next_fetch_at,reference_date)
values ('refetch-test','event','ok','ok','ok','2026-06-01 12:00Z','2026-06-02 12:00Z','2026-06-05'),
  ('refetch-test','event','pending','pending','pending','2026-06-01 12:00Z','2026-06-02 12:00Z','2026-06-05'),
  ('refetch-test','event','transient','transient','ok','2026-06-01 12:00Z','2026-06-02 12:00Z','2026-06-05');
update crawler.frontier set reference_date = '2020-01-01' where federation='refetch-test';
select tap.is((select next_fetch_at from crawler.frontier where federation='refetch-test' and key='ok'),
  '2027-06-01 12:00Z'::timestamptz, 'date correction recalculates from the actual fetch');
select tap.is((select process_status::text from crawler.frontier where federation='refetch-test' and key='ok'),
  'ok', 'date correction preserves processing status');
select tap.is((select next_fetch_at from crawler.frontier where federation='refetch-test' and key='pending'),
  '2026-06-02 12:00Z'::timestamptz, 'date update preserves an explicit fetch request');
select tap.is((select next_fetch_at from crawler.frontier where federation='refetch-test' and key='transient'),
  '2026-06-02 12:00Z'::timestamptz, 'date update preserves transient backoff');
update crawler.frontier set reference_date = '2026-06-05' where federation='refetch-test' and key='ok';
select tap.is((select next_fetch_at from crawler.frontier where federation='refetch-test' and key='ok'),
  '2026-06-02 12:00Z'::timestamptz, 'correction back to a recent date shortens the interval');
update crawler.refetch_policy set fetch_interval='2 days'
where federation='refetch-test' and reference_offset=interval '-1 week';
select tap.is(crawler.recalculate_fetch_times('refetch-test','event'),1,'policy edit recalculates existing successful frontiers');
select tap.is((select next_fetch_at from crawler.frontier where federation='refetch-test' and key='ok'),
  '2026-06-03 12:00Z'::timestamptz, 'policy edit keeps the original fetch time as its anchor');
select tap.is(crawler.recalculate_fetch_times('refetch-test','event'),0,'recalculation is repeatable');
select tap.is((select count(*)::int from crawler.frontier where federation='refetch-test' and fetch_status<>'ok' and next_fetch_at='2026-06-02 12:00Z'),
  2,'recalculation preserves requests and retries');
select tap.throws_ok($$insert into crawler.refetch_policy values ('refetch-test','event',null,'1 day')$$,
  '23505', null, 'a kind has only one default rule');
select tap.throws_ok($$insert into crawler.refetch_policy values ('refetch-test','bad',null,'0 seconds')$$,
  '23514', null, 'fetch interval must be positive');

set local role system_admin;
select tap.ok((select count(*) > 0 from crawler.refetch_policy), 'system admin can inspect policies');
reset role;
set local role anonymous;
select tap.throws_ok($$select * from crawler.refetch_policy$$, '42501', null,
  'anonymous users cannot read internal crawler policies');
reset role;

select * from tap.finish();
rollback;
