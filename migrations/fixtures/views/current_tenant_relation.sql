create or replace view app_private.current_tenant_relation as
select * from app_private.tenant_relation
where tenant_id = (select current_tenant_id()) and is_current;

grant select on app_private.current_tenant_relation to anonymous;
