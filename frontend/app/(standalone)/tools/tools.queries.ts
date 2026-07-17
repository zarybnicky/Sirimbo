/** Types generated for queries found in "app/(standalone)/tools/tools.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type DateOrString = Date | string;

export type stringArray = (string)[];

/** 'CompetitionEventLocations' parameters type */
export interface ICompetitionEventLocationsParams {
  federation: string;
  fromDate: DateOrString;
  toDate: DateOrString;
}

/** 'CompetitionEventLocations' return type */
export interface ICompetitionEventLocationsResult {
  location: string | null;
  name: string | null;
  startDate: string;
  venueLat: number;
  venueLng: number;
}

/** 'CompetitionEventLocations' query type */
export interface ICompetitionEventLocationsQuery {
  params: ICompetitionEventLocationsParams;
  result: ICompetitionEventLocationsResult;
}

const competitionEventLocationsIR: any = {"usedParamSet":{"federation":true,"fromDate":true,"toDate":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":206,"b":217}]},{"name":"fromDate","required":true,"transform":{"type":"scalar"},"locs":[{"a":246,"b":255}]},{"name":"toDate","required":true,"transform":{"type":"scalar"},"locs":[{"a":267,"b":274}]}],"statement":"select\n    e.name as \"name\",\n    e.location as \"location\",\n    e.start_date::text as \"startDate!\",\n    e.venue_lat as \"venueLat!\",\n    e.venue_lng as \"venueLng!\"\nfrom federated.event e\nwhere e.federation = :federation!\n  and e.start_date between :fromDate!::date and :toDate!::date\n  and e.venue_lat is not null\n  and e.venue_lng is not null\norder by e.start_date"};

/**
 * Query generated from SQL:
 * ```
 * select
 *     e.name as "name",
 *     e.location as "location",
 *     e.start_date::text as "startDate!",
 *     e.venue_lat as "venueLat!",
 *     e.venue_lng as "venueLng!"
 * from federated.event e
 * where e.federation = :federation!
 *   and e.start_date between :fromDate!::date and :toDate!::date
 *   and e.venue_lat is not null
 *   and e.venue_lng is not null
 * order by e.start_date
 * ```
 */
export const competitionEventLocations = new PreparedQuery<ICompetitionEventLocationsParams,ICompetitionEventLocationsResult>(competitionEventLocationsIR);


/** 'CompetitionSankeyLinks' parameters type */
export interface ICompetitionSankeyLinksParams {
  classes: stringArray;
  disciplines: stringArray;
  firstYear: number;
  lastYear: number;
  mergeInactive: boolean;
}

/** 'CompetitionSankeyLinks' return type */
export interface ICompetitionSankeyLinksResult {
  discipline: string;
  fromClass: string | null;
  fromState: string;
  fromYear: number;
  kind: string;
  toClass: string | null;
  toState: string;
  toYear: number;
  value: number;
}

/** 'CompetitionSankeyLinks' query type */
export interface ICompetitionSankeyLinksQuery {
  params: ICompetitionSankeyLinksParams;
  result: ICompetitionSankeyLinksResult;
}

const competitionSankeyLinksIR: any = {"usedParamSet":{"disciplines":true,"classes":true,"lastYear":true,"mergeInactive":true,"firstYear":true},"params":[{"name":"disciplines","required":true,"transform":{"type":"scalar"},"locs":[{"a":588,"b":600}]},{"name":"classes","required":true,"transform":{"type":"scalar"},"locs":[{"a":636,"b":644},{"a":3545,"b":3553},{"a":3728,"b":3736}]},{"name":"lastYear","required":true,"transform":{"type":"scalar"},"locs":[{"a":1019,"b":1028},{"a":2430,"b":2439},{"a":2522,"b":2531},{"a":3108,"b":3117}]},{"name":"mergeInactive","required":true,"transform":{"type":"scalar"},"locs":[{"a":2722,"b":2736},{"a":2885,"b":2899}]},{"name":"firstYear","required":true,"transform":{"type":"scalar"},"locs":[{"a":3073,"b":3083}]}],"statement":"with base as (\n    select\n        cm.person_id,\n        extract(year from comp.start_date)::int as period_year,\n        c.discipline as discipline,\n        c.class as class,\n        federated.class_rank(c.class) as class_rank,\n        comp.start_date as date,\n        comp.category_id,\n        cr.competitor_id\n    from federated.competition_result cr\n    join federated.competition comp on cr.competition_id = comp.id\n    join federated.competitor_component cm on cm.competitor_id = cr.competitor_id\n    join federated.category c on c.id = comp.category_id\n    where c.discipline = any (:disciplines!::text[])\n      and c.class = any (:classes!::text[])\n),\nobserved as (\n    select r.person_id, r.period_year, r.discipline, r.class\n    from (\n        select b.*, row_number() over (\n          partition by b.person_id, b.period_year, b.discipline\n          order by b.class_rank desc nulls last, b.date desc nulls last, b.category_id, b.competitor_id\n        ) as rn\n        from base b\n        where b.period_year <= :lastYear!::int\n    ) r\n    where r.rn = 1\n),\nwindowed as (\n    select\n        o.*,\n        row_number() over person_timeline as observed_n,\n        lead(o.period_year) over person_timeline as next_year,\n        lead(o.class) over person_timeline as next_class\n    from observed o\n    window person_timeline as (partition by o.person_id, o.discipline order by o.period_year)\n),\nperson_links(from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind) as (\n    select\n        w.period_year - 1,\n        'inactive',\n        w.class,\n        w.period_year,\n        'active',\n        w.class,\n        w.discipline,\n        'entry'\n    from windowed w\n    where w.observed_n = 1\n\n    union all\n\n    select\n        y.from_year,\n        case when y.from_year = w.period_year then 'active' else 'inactive' end,\n        w.class,\n        y.from_year + 1,\n        case when w.next_year = y.from_year + 1 then 'active' else 'inactive' end,\n        case when w.next_year = y.from_year + 1 then w.next_class else w.class end,\n        w.discipline,\n        case\n            when w.next_year = w.period_year + 1 then 'flow'\n            when y.from_year = w.period_year then 'dropout'\n            when w.next_year = y.from_year + 1 then 'return'\n            else 'inactive'\n        end\n    from windowed w\n    cross join lateral generate_series(w.period_year, coalesce(w.next_year, greatest(w.period_year, :lastYear!::int)) - 1) as y(from_year)\n    where w.next_year is not null or w.period_year < :lastYear!::int\n),\n-- with mergeInactive, inactive endpoints lose their class and collapse into one node per year\nmerged as (\n    select\n        pl.from_year,\n        pl.from_state,\n        case when :mergeInactive!::boolean and pl.from_state = 'inactive' then null else pl.from_class end as from_class,\n        pl.to_year,\n        pl.to_state,\n        case when :mergeInactive!::boolean and pl.to_state = 'inactive' then null else pl.to_class end as to_class,\n        pl.discipline,\n        pl.kind\n    from person_links pl\n    where pl.from_year >= :firstYear!::int and pl.to_year <= :lastYear!::int\n)\nselect\n    from_year as \"fromYear!\",\n    from_state as \"fromState!\",\n    from_class as \"fromClass\",\n    to_year as \"toYear!\",\n    to_state as \"toState!\",\n    to_class as \"toClass\",\n    discipline as \"discipline!\",\n    kind as \"kind!\",\n    count(*)::int as \"value!\"\nfrom merged\ngroup by from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind\norder by\n    from_year,\n    coalesce(array_position(:classes!::text[], from_class), 9999),\n    array_position(array['entry','flow','dropout','inactive','return']::text[], kind),\n    discipline,\n    to_year,\n    coalesce(array_position(:classes!::text[], to_class), 9999)"};

/**
 * Query generated from SQL:
 * ```
 * with base as (
 *     select
 *         cm.person_id,
 *         extract(year from comp.start_date)::int as period_year,
 *         c.discipline as discipline,
 *         c.class as class,
 *         federated.class_rank(c.class) as class_rank,
 *         comp.start_date as date,
 *         comp.category_id,
 *         cr.competitor_id
 *     from federated.competition_result cr
 *     join federated.competition comp on cr.competition_id = comp.id
 *     join federated.competitor_component cm on cm.competitor_id = cr.competitor_id
 *     join federated.category c on c.id = comp.category_id
 *     where c.discipline = any (:disciplines!::text[])
 *       and c.class = any (:classes!::text[])
 * ),
 * observed as (
 *     select r.person_id, r.period_year, r.discipline, r.class
 *     from (
 *         select b.*, row_number() over (
 *           partition by b.person_id, b.period_year, b.discipline
 *           order by b.class_rank desc nulls last, b.date desc nulls last, b.category_id, b.competitor_id
 *         ) as rn
 *         from base b
 *         where b.period_year <= :lastYear!::int
 *     ) r
 *     where r.rn = 1
 * ),
 * windowed as (
 *     select
 *         o.*,
 *         row_number() over person_timeline as observed_n,
 *         lead(o.period_year) over person_timeline as next_year,
 *         lead(o.class) over person_timeline as next_class
 *     from observed o
 *     window person_timeline as (partition by o.person_id, o.discipline order by o.period_year)
 * ),
 * person_links(from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind) as (
 *     select
 *         w.period_year - 1,
 *         'inactive',
 *         w.class,
 *         w.period_year,
 *         'active',
 *         w.class,
 *         w.discipline,
 *         'entry'
 *     from windowed w
 *     where w.observed_n = 1
 * 
 *     union all
 * 
 *     select
 *         y.from_year,
 *         case when y.from_year = w.period_year then 'active' else 'inactive' end,
 *         w.class,
 *         y.from_year + 1,
 *         case when w.next_year = y.from_year + 1 then 'active' else 'inactive' end,
 *         case when w.next_year = y.from_year + 1 then w.next_class else w.class end,
 *         w.discipline,
 *         case
 *             when w.next_year = w.period_year + 1 then 'flow'
 *             when y.from_year = w.period_year then 'dropout'
 *             when w.next_year = y.from_year + 1 then 'return'
 *             else 'inactive'
 *         end
 *     from windowed w
 *     cross join lateral generate_series(w.period_year, coalesce(w.next_year, greatest(w.period_year, :lastYear!::int)) - 1) as y(from_year)
 *     where w.next_year is not null or w.period_year < :lastYear!::int
 * ),
 * -- with mergeInactive, inactive endpoints lose their class and collapse into one node per year
 * merged as (
 *     select
 *         pl.from_year,
 *         pl.from_state,
 *         case when :mergeInactive!::boolean and pl.from_state = 'inactive' then null else pl.from_class end as from_class,
 *         pl.to_year,
 *         pl.to_state,
 *         case when :mergeInactive!::boolean and pl.to_state = 'inactive' then null else pl.to_class end as to_class,
 *         pl.discipline,
 *         pl.kind
 *     from person_links pl
 *     where pl.from_year >= :firstYear!::int and pl.to_year <= :lastYear!::int
 * )
 * select
 *     from_year as "fromYear!",
 *     from_state as "fromState!",
 *     from_class as "fromClass",
 *     to_year as "toYear!",
 *     to_state as "toState!",
 *     to_class as "toClass",
 *     discipline as "discipline!",
 *     kind as "kind!",
 *     count(*)::int as "value!"
 * from merged
 * group by from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind
 * order by
 *     from_year,
 *     coalesce(array_position(:classes!::text[], from_class), 9999),
 *     array_position(array['entry','flow','dropout','inactive','return']::text[], kind),
 *     discipline,
 *     to_year,
 *     coalesce(array_position(:classes!::text[], to_class), 9999)
 * ```
 */
export const competitionSankeyLinks = new PreparedQuery<ICompetitionSankeyLinksParams,ICompetitionSankeyLinksResult>(competitionSankeyLinksIR);


