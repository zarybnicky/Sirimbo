# public.event_instance_trainer

## Description

@omit create,update,delete  
@simpleCollections only

## Columns

| Name | Type | Default | Nullable | Children | Parents | Comment |
| ---- | ---- | ------- | -------- | -------- | ------- | ------- |
| id | bigint |  | false |  |  |  |
| tenant_id | bigint | current_tenant_id() | false |  | [public.tenant](public.tenant.md) |  |
| instance_id | bigint |  | false |  | [public.event_instance](public.event_instance.md) |  |
| person_id | bigint |  | false |  | [public.person](public.person.md) |  |
| created_at | timestamp with time zone | now() | false |  |  |  |
| updated_at | timestamp with time zone | now() | false |  |  |  |
| lesson_price | price | NULL::price_type | true |  |  |  |

## Constraints

| Name | Type | Definition |
| ---- | ---- | ---------- |
| event_instance_trainer_instance_id_fkey | FOREIGN KEY | FOREIGN KEY (instance_id) REFERENCES event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE |
| event_instance_trainer_pkey | PRIMARY KEY | PRIMARY KEY (id) |
| event_instance_trainer_person_id_fkey | FOREIGN KEY | FOREIGN KEY (person_id) REFERENCES person(id) ON UPDATE CASCADE ON DELETE CASCADE |
| event_instance_trainer_tenant_id_fkey | FOREIGN KEY | FOREIGN KEY (tenant_id) REFERENCES tenant(id) ON UPDATE CASCADE ON DELETE CASCADE |

## Indexes

| Name | Definition |
| ---- | ---------- |
| event_instance_trainer_pkey | CREATE UNIQUE INDEX event_instance_trainer_pkey ON public.event_instance_trainer USING btree (id) |
| event_instance_trainer_instance_id_idx | CREATE INDEX event_instance_trainer_instance_id_idx ON public.event_instance_trainer USING btree (instance_id) |
| event_instance_trainer_person_id_idx | CREATE INDEX event_instance_trainer_person_id_idx ON public.event_instance_trainer USING btree (person_id) |
| event_instance_trainer_tenant_id_idx | CREATE INDEX event_instance_trainer_tenant_id_idx ON public.event_instance_trainer USING btree (tenant_id) |

## Triggers

| Name | Definition |
| ---- | ---------- |
| _100_timestamps | CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps() |

## Relations

![er](public.event_instance_trainer.svg)

---

> Generated by [tbls](https://github.com/k1LoW/tbls)