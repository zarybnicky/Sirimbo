# public.platby_group_skupina

## Description

@omit

## Columns

| Name | Type | Default | Nullable | Extra Definition | Children | Parents | Comment |
| ---- | ---- | ------- | -------- | ---------------- | -------- | ------- | ------- |
| pgs_id | bigint | nextval('platby_group_skupina_pgs_id_seq'::regclass) | false |  |  |  |  |
| pgs_id_skupina | bigint |  | false |  |  | [public.cohort](public.cohort.md) |  |
| pgs_id_group | bigint |  | false |  |  | [public.platby_group](public.platby_group.md) |  |
| id | bigint |  | false | GENERATED ALWAYS AS pgs_id STORED |  |  |  |
| tenant_id | bigint | current_tenant_id() | false |  |  | [public.tenant](public.tenant.md) |  |

## Constraints

| Name | Type | Definition |
| ---- | ---- | ---------- |
| platby_group_skupina_pgs_id_skupina_fkey | FOREIGN KEY | FOREIGN KEY (pgs_id_skupina) REFERENCES cohort(id) |
| platby_group_skupina_pgs_id_group_fkey | FOREIGN KEY | FOREIGN KEY (pgs_id_group) REFERENCES platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT |
| idx_24707_primary | PRIMARY KEY | PRIMARY KEY (pgs_id) |
| platby_group_skupina_unique_id | UNIQUE | UNIQUE (id) |
| platby_group_skupina_tenant_id_fkey | FOREIGN KEY | FOREIGN KEY (tenant_id) REFERENCES tenant(id) ON DELETE CASCADE |

## Indexes

| Name | Definition |
| ---- | ---------- |
| idx_24707_primary | CREATE UNIQUE INDEX idx_24707_primary ON public.platby_group_skupina USING btree (pgs_id) |
| platby_group_skupina_unique_id | CREATE UNIQUE INDEX platby_group_skupina_unique_id ON public.platby_group_skupina USING btree (id) |
| idx_24707_pgs_id_skupina | CREATE UNIQUE INDEX idx_24707_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group) |

## Relations

![er](public.platby_group_skupina.svg)

---

> Generated by [tbls](https://github.com/k1LoW/tbls)
