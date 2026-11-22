-- Serialize installation/backfill with source writes, including on reruns.
lock table tenant_membership, tenant_trainer, tenant_administrator
  in share row exclusive mode;

do $$ begin
  if to_regtype('app_private.tenant_role') is null then
    create type app_private.tenant_role as enum ('member', 'trainer', 'administrator');
  end if;
end $$;

create table if not exists app_private.tenant_relation (
  tenant_id bigint not null references tenant(id) on delete cascade,
  person_id bigint not null references person(id) on delete cascade,
  current_roles app_private.tenant_role[] not null default '{}',
  expired_roles app_private.tenant_role[] not null default '{}',
  pending_roles app_private.tenant_role[] not null default '{}',
  is_current boolean generated always as (cardinality(current_roles) > 0) stored,
  is_former boolean generated always as
    (cardinality(current_roles) = 0 and cardinality(expired_roles) > 0) stored,
  primary key (tenant_id, person_id)
);
create index if not exists tenant_relation_person_idx
  on app_private.tenant_relation (person_id, tenant_id);

--!include functions/tenant_relation.sql

-- The same rebuild handles initial population, repairs and clearing stale roles.
select app_private.rebuild_tenant_relation(tenant_id, person_id)
from (
  select tenant_id, person_id from tenant_membership
  union select tenant_id, person_id from tenant_trainer
  union select tenant_id, person_id from tenant_administrator
  union select tenant_id, person_id from app_private.tenant_relation
) pairs order by tenant_id, person_id;
