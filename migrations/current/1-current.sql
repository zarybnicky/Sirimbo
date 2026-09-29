do $$
begin
  if exists (select 1 from pg_class where oid = to_regclass('public.tenant_location') and relkind = 'r') then
    alter table tenant_location rename to location;
  end if;
end;
$$;

create or replace view tenant_location with (security_invoker = true)
  as select * from location;

comment on table location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection -queryField:resource:connection';

comment on view tenant_location is '@primaryKey id
@unique tenant_id,id
@foreignKey (tenant_id) references tenant(id)
@foreignKey (tenant_id,cover_image_id) references file(tenant_id,id)|@fieldName coverImage|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@simpleCollections only
@behavior -insert -update -delete -query:resource:list -query:resource:connection -queryField:resource:connection';

comment on column tenant_location.name is '@notNull';
comment on column tenant_location.description is '@notNull';
comment on column tenant_location.tenant_id is '@notNull';
comment on column tenant_location.created_at is '@notNull';
comment on column tenant_location.updated_at is '@notNull';
comment on column tenant_location.show_in_lists is '@notNull';
comment on column tenant_location.ordering is '@notNull';

comment on table access_event is '@omit create,update,delete
@simpleCollections only
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName tenantLocation|@foreignFieldName accessEvents';
comment on table event_instance is '@omit create
@simpleCollections only
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName location|@foreignFieldName eventInstances';
comment on table tenant_location_image is '@omit create,update,delete
@simpleCollections only
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName location|@foreignFieldName images';

-- Expose Location separately while legacy readers continue through the compatibility view.
comment on constraint access_event_location_fkey on access_event is '@fieldName locationRecord
@foreignFieldName accessEvents';
comment on constraint event_instance_location_fkey on event_instance is '@fieldName locationRecord
@foreignFieldName eventInstances';
comment on constraint tenant_location_image_location_fk on tenant_location_image is '@fieldName locationRecord
@foreignFieldName images';

revoke all on tenant_location from public, anonymous;
grant select on tenant_location to anonymous;

--!include functions/upsert_location.sql
--!include functions/visible_file_ids.sql
