# public.location

## Description

## Columns

| Name | Type | Default | Nullable | Children | Parents | Comment |
| ---- | ---- | ------- | -------- | -------- | ------- | ------- |
| id | bigint |  | false | [public.location_attachment](public.location_attachment.md) [public.room](public.room.md) |  |  |
| name | text |  | false |  |  |  |
| description | jsonb |  | false |  |  |  |
| address | address_domain |  | true |  |  |  |

## Constraints

| Name | Type | Definition |
| ---- | ---- | ---------- |
| location_pkey | PRIMARY KEY | PRIMARY KEY (id) |

## Indexes

| Name | Definition |
| ---- | ---------- |
| location_pkey | CREATE UNIQUE INDEX location_pkey ON public.location USING btree (id) |

## Relations

![er](public.location.svg)

---

> Generated by [tbls](https://github.com/k1LoW/tbls)