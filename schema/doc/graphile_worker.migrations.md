# graphile_worker.migrations

## Description

## Columns

| Name | Type | Default | Nullable | Children | Parents | Comment |
| ---- | ---- | ------- | -------- | -------- | ------- | ------- |
| id | integer |  | false |  |  |  |
| ts | timestamp with time zone | now() | false |  |  |  |
| breaking | boolean | false | false |  |  |  |

## Constraints

| Name | Type | Definition |
| ---- | ---- | ---------- |
| migrations_pkey | PRIMARY KEY | PRIMARY KEY (id) |

## Indexes

| Name | Definition |
| ---- | ---------- |
| migrations_pkey | CREATE UNIQUE INDEX migrations_pkey ON graphile_worker.migrations USING btree (id) |

## Relations

![er](graphile_worker.migrations.svg)

---

> Generated by [tbls](https://github.com/k1LoW/tbls)
