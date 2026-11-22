begin;
set local search_path = public, tap;
select tap.plan(13);
insert into tenant (id, name) values (1000, 'Relations'), (1001, 'Moved relations') on conflict do nothing;
insert into person (id, first_name, last_name, gender, nationality) overriding system value
values (200001, 'Relation', 'Test', 'unspecified', ''), (200002, 'Moved', 'Test', 'unspecified', '');

insert into tenant_membership (tenant_id, person_id, since, until) values
  (1000, 200001, now() - interval '3 years', now() - interval '2 years'),
  (1000, 200001, now() - interval '1 year', null);
insert into tenant_trainer (tenant_id, person_id, since, until) values
  (1000, 200001, now() - interval '1 year', now() - interval '1 day');
insert into tenant_administrator (tenant_id, person_id, since) values
  (1000, 200001, now() + interval '1 year');
select tap.is((select current_roles::text from app_private.tenant_relation where person_id = 200001), '{member}', 'current roles');
select tap.is((select expired_roles::text from app_private.tenant_relation where person_id = 200001), '{member,trainer}', 'expired roles survive alongside current roles');
select tap.is((select pending_roles::text from app_private.tenant_relation where person_id = 200001), '{administrator}', 'pending roles');
select tap.ok((select is_current and not is_former from app_private.tenant_relation where person_id = 200001), 'expired role does not make a current person former');

create temp table original_relation as select ctid as row_tid from app_private.tenant_relation where person_id = 200001;
update tenant_trainer set description = 'irrelevant' where tenant_id = 1000 and person_id = 200001;
select tap.is((select ctid::text from app_private.tenant_relation where person_id = 200001), (select row_tid::text from original_relation), 'unrelated writes do not rewrite projection');

update tenant_membership set until = now() - interval '1 hour' where tenant_id = 1000 and person_id = 200001 and until is null;
select tap.ok((select is_former and not is_current from app_private.tenant_relation where person_id = 200001), 'date-triggered expiry makes the person former');
delete from tenant_membership where tenant_id = 1000 and person_id = 200001;
delete from tenant_trainer where tenant_id = 1000 and person_id = 200001;
select tap.ok((select not is_former and not is_current from app_private.tenant_relation where person_id = 200001), 'pending-only person is neither current nor former');

update tenant_administrator set tenant_id = 1001, person_id = 200002 where tenant_id = 1000 and person_id = 200001;
select tap.ok((select current_roles = '{}' and expired_roles = '{}' and pending_roles = '{}' from app_private.tenant_relation where person_id = 200001), 'moving the last source leaves an empty row');
select tap.is((select pending_roles::text from app_private.tenant_relation where tenant_id = 1001 and person_id = 200002), '{administrator}', 'move builds new pair');

savepoint before_activation;
update tenant_administrator set status = 'active' where tenant_id = 1001 and person_id = 200002;
select tap.is((select current_roles::text from app_private.tenant_relation where person_id = 200002), '{administrator}', 'status-only changes refresh the projection');
rollback to before_activation;
select tap.is((select pending_roles::text from app_private.tenant_relation where person_id = 200002), '{administrator}', 'projection rolls back with source changes');
delete from tenant_administrator where tenant_id = 1001 and person_id = 200002;
select tap.ok((select current_roles = '{}' and expired_roles = '{}' and pending_roles = '{}' from app_private.tenant_relation where person_id = 200002), 'last deletion leaves an empty row');
insert into tenant_membership (tenant_id, person_id, since) values (1000, 200001, now() + interval '1 year');
insert into tenant_trainer (tenant_id, person_id, since) values (1000, 200001, now() + interval '1 year');
delete from person where id = 200001;
select tap.is((select count(*)::int from app_private.tenant_relation where person_id = 200001), 0, 'parent deletion cannot recreate a projection during cascades');
select * from tap.finish();
rollback;
