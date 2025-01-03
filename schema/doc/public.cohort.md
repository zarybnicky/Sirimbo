# public.cohort

## Description

@simpleCollections only

## Columns

| Name | Type | Default | Nullable | Children | Parents | Comment |
| ---- | ---- | ------- | -------- | -------- | ------- | ------- |
| id | bigint |  | false | [public.event_target_cohort](public.event_target_cohort.md) [public.cohort_subscription](public.cohort_subscription.md) [public.cohort_membership](public.cohort_membership.md) [public.platby_group_skupina](public.platby_group_skupina.md) [public.upozorneni_skupiny](public.upozorneni_skupiny.md) |  |  |
| tenant_id | bigint | current_tenant_id() | false |  | [public.tenant](public.tenant.md) |  |
| cohort_group_id | bigint |  | true |  | [public.cohort_group](public.cohort_group.md) |  |
| name | text |  | false |  |  |  |
| description | text | ''::text | false |  |  |  |
| color_rgb | text |  | false |  |  |  |
| location | text | ''::text | false |  |  |  |
| is_visible | boolean | true | false |  |  |  |
| ordering | integer | 1 | false |  |  |  |

## Constraints

| Name | Type | Definition |
| ---- | ---- | ---------- |
| cohort_cohort_group_id_fkey | FOREIGN KEY | FOREIGN KEY (cohort_group_id) REFERENCES cohort_group(id) ON DELETE SET NULL |
| cohort_pkey | PRIMARY KEY | PRIMARY KEY (id) |
| cohort_tenant_id_fkey | FOREIGN KEY | FOREIGN KEY (tenant_id) REFERENCES tenant(id) ON DELETE CASCADE |

## Indexes

| Name | Definition |
| ---- | ---------- |
| cohort_pkey | CREATE UNIQUE INDEX cohort_pkey ON public.cohort USING btree (id) |

## Relations

![er](public.cohort.svg)

---

> Generated by [tbls](https://github.com/k1LoW/tbls)
