/** Types generated for queries found in "lib/event-actions.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type DateOrString = Date | string;

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

/** 'CampCloneEvents' parameters type */
export interface ICampCloneEventsParams {
  since: DateOrString;
  sourceId: NumberOrString;
}

/** 'CampCloneEvents' return type */
export interface ICampCloneEventsResult {
  id: string;
  parent_id: string | null;
  shift: string;
}

/** 'CampCloneEvents' query type */
export interface ICampCloneEventsQuery {
  params: ICampCloneEventsParams;
  result: ICampCloneEventsResult;
}

const campCloneEventsIR: any = {"usedParamSet":{"sourceId":true,"since":true},"params":[{"name":"sourceId","required":true,"transform":{"type":"scalar"},"locs":[{"a":93,"b":102}]},{"name":"since","required":true,"transform":{"type":"scalar"},"locs":[{"a":328,"b":334}]}],"statement":"with recursive tree as (\n  select id, parent_id, since, 0 as depth\n  from event\n  where id = :sourceId! and type = 'camp'\n  union all\n  select child.id, child.parent_id, child.since, parent.depth + 1\n  from event child join tree parent on child.parent_id = parent.id\n)\nselect id::text as \"id!\", parent_id::text as parent_id,\n  (:since!::timestamptz - (select since from tree where depth = 0))::text as \"shift!\"\nfrom tree\norder by depth, since, id"};

/**
 * Query generated from SQL:
 * ```
 * with recursive tree as (
 *   select id, parent_id, since, 0 as depth
 *   from event
 *   where id = :sourceId! and type = 'camp'
 *   union all
 *   select child.id, child.parent_id, child.since, parent.depth + 1
 *   from event child join tree parent on child.parent_id = parent.id
 * )
 * select id::text as "id!", parent_id::text as parent_id,
 *   (:since!::timestamptz - (select since from tree where depth = 0))::text as "shift!"
 * from tree
 * order by depth, since, id
 * ```
 */
export const campCloneEvents = new PreparedQuery<ICampCloneEventsParams,ICampCloneEventsResult>(campCloneEventsIR);


/** 'CloneCampEvent' parameters type */
export interface ICloneCampEventParams {
  copyCohorts: boolean;
  copyTrainers: boolean;
  parentId?: NumberOrString | null | void;
  shift: string;
  sourceId: NumberOrString;
}

/** 'CloneCampEvent' return type */
export interface ICloneCampEventResult {
  id: string;
}

/** 'CloneCampEvent' query type */
export interface ICloneCampEventQuery {
  params: ICloneCampEventParams;
  result: ICloneCampEventResult;
}

const cloneCampEventIR: any = {"usedParamSet":{"parentId":true,"shift":true,"copyTrainers":true,"copyCohorts":true,"sourceId":true},"params":[{"name":"parentId","required":false,"transform":{"type":"scalar"},"locs":[{"a":211,"b":219}]},{"name":"shift","required":true,"transform":{"type":"scalar"},"locs":[{"a":615,"b":621},{"a":686,"b":692},{"a":954,"b":960},{"a":998,"b":1004}]},{"name":"copyTrainers","required":true,"transform":{"type":"scalar"},"locs":[{"a":1110,"b":1123}]},{"name":"copyCohorts","required":true,"transform":{"type":"scalar"},"locs":[{"a":1328,"b":1340}]},{"name":"sourceId","required":true,"transform":{"type":"scalar"},"locs":[{"a":1490,"b":1499}]}],"statement":"select saved.id::text as \"id!\"\nfrom event source\ncross join lateral save_events(\n  details => jsonb_populate_record(\n    null::event_details_input,\n    to_jsonb(source) || jsonb_build_object(\n      'parent_id', :parentId::bigint,\n      'name', case when source.type = 'lesson' then null else source.name end,\n      'capacity', coalesce(source.capacity, 0),\n      'is_locked', null,\n      'registration_period', case\n        when source.registration_period is null or isempty(source.registration_period)\n          then source.registration_period\n        else tstzrange(\n          lower(source.registration_period) + :shift!::text::interval,\n          upper(source.registration_period) + :shift!::text::interval,\n          (case when lower_inc(source.registration_period) then '[' else '(' end)\n          || (case when upper_inc(source.registration_period) then ']' else ')' end)\n        )\n      end\n    )),\n  events => array[\n    row(null, source.since + :shift!::text::interval,\n    source.until + :shift!::text::interval,\n    false, '{}'::event_registration_input[])::event_input\n  ],\n  trainers => case when :copyTrainers!::boolean then array(\n    select row(person_id, lessons_offered)::event_trainer_input\n    from event_trainer where event_id = source.id\n  ) else '{}'::event_trainer_input[] end,\n  cohort_ids => case when :copyCohorts!::boolean then array(\n    select cohort_id from event_target_cohort where event_id = source.id\n  ) else '{}'::bigint[] end\n) saved\nwhere source.id = :sourceId!"};

/**
 * Query generated from SQL:
 * ```
 * select saved.id::text as "id!"
 * from event source
 * cross join lateral save_events(
 *   details => jsonb_populate_record(
 *     null::event_details_input,
 *     to_jsonb(source) || jsonb_build_object(
 *       'parent_id', :parentId::bigint,
 *       'name', case when source.type = 'lesson' then null else source.name end,
 *       'capacity', coalesce(source.capacity, 0),
 *       'is_locked', null,
 *       'registration_period', case
 *         when source.registration_period is null or isempty(source.registration_period)
 *           then source.registration_period
 *         else tstzrange(
 *           lower(source.registration_period) + :shift!::text::interval,
 *           upper(source.registration_period) + :shift!::text::interval,
 *           (case when lower_inc(source.registration_period) then '[' else '(' end)
 *           || (case when upper_inc(source.registration_period) then ']' else ')' end)
 *         )
 *       end
 *     )),
 *   events => array[
 *     row(null, source.since + :shift!::text::interval,
 *     source.until + :shift!::text::interval,
 *     false, '{}'::event_registration_input[])::event_input
 *   ],
 *   trainers => case when :copyTrainers!::boolean then array(
 *     select row(person_id, lessons_offered)::event_trainer_input
 *     from event_trainer where event_id = source.id
 *   ) else '{}'::event_trainer_input[] end,
 *   cohort_ids => case when :copyCohorts!::boolean then array(
 *     select cohort_id from event_target_cohort where event_id = source.id
 *   ) else '{}'::bigint[] end
 * ) saved
 * where source.id = :sourceId!
 * ```
 */
export const cloneCampEvent = new PreparedQuery<ICloneCampEventParams,ICloneCampEventResult>(cloneCampEventIR);


/** 'CopyEventMetadata' parameters type */
export interface ICopyEventMetadataParams {
  copyIds: NumberOrStringArray;
  sourceIds: NumberOrStringArray;
}

/** 'CopyEventMetadata' return type */
export type ICopyEventMetadataResult = void;

/** 'CopyEventMetadata' query type */
export interface ICopyEventMetadataQuery {
  params: ICopyEventMetadataParams;
  result: ICopyEventMetadataResult;
}

const copyEventMetadataIR: any = {"usedParamSet":{"sourceIds":true,"copyIds":true},"params":[{"name":"sourceIds","required":true,"transform":{"type":"scalar"},"locs":[{"a":141,"b":151}]},{"name":"copyIds","required":true,"transform":{"type":"scalar"},"locs":[{"a":164,"b":172}]}],"statement":"update event copy\nset summary = source.summary,\n    description = source.description,\n    cover_image_id = source.cover_image_id\nfrom unnest(:sourceIds!::bigint[], :copyIds!::bigint[]) mapping(source_id, copy_id)\njoin event source on source.id = mapping.source_id\nwhere copy.id = mapping.copy_id"};

/**
 * Query generated from SQL:
 * ```
 * update event copy
 * set summary = source.summary,
 *     description = source.description,
 *     cover_image_id = source.cover_image_id
 * from unnest(:sourceIds!::bigint[], :copyIds!::bigint[]) mapping(source_id, copy_id)
 * join event source on source.id = mapping.source_id
 * where copy.id = mapping.copy_id
 * ```
 */
export const copyEventMetadata = new PreparedQuery<ICopyEventMetadataParams,ICopyEventMetadataResult>(copyEventMetadataIR);


/** 'PopulateEventCohorts' parameters type */
export interface IPopulateEventCohortsParams {
  ids: NumberOrStringArray;
}

/** 'PopulateEventCohorts' return type */
export interface IPopulateEventCohortsResult {
  reconcile_event_cohort_registrations: undefined | null;
}

/** 'PopulateEventCohorts' query type */
export interface IPopulateEventCohortsQuery {
  params: IPopulateEventCohortsParams;
  result: IPopulateEventCohortsResult;
}

const populateEventCohortsIR: any = {"usedParamSet":{"ids":true},"params":[{"name":"ids","required":true,"transform":{"type":"scalar"},"locs":[{"a":56,"b":60}]}],"statement":"select app_private.reconcile_event_cohort_registrations(:ids!::bigint[])"};

/**
 * Query generated from SQL:
 * ```
 * select app_private.reconcile_event_cohort_registrations(:ids!::bigint[])
 * ```
 */
export const populateEventCohorts = new PreparedQuery<IPopulateEventCohortsParams,IPopulateEventCohortsResult>(populateEventCohortsIR);


/** 'EventSeriesEvents' parameters type */
export interface IEventSeriesEventsParams {
  id: NumberOrString;
}

/** 'EventSeriesEvents' return type */
export interface IEventSeriesEventsResult {
  id: string;
}

/** 'EventSeriesEvents' query type */
export interface IEventSeriesEventsQuery {
  params: IEventSeriesEventsParams;
  result: IEventSeriesEventsResult;
}

const eventSeriesEventsIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":39,"b":42}]}],"statement":"select id from event where series_id = :id! limit 2"};

/**
 * Query generated from SQL:
 * ```
 * select id from event where series_id = :id! limit 2
 * ```
 */
export const eventSeriesEvents = new PreparedQuery<IEventSeriesEventsParams,IEventSeriesEventsResult>(eventSeriesEventsIR);


/** 'DeleteEventRecursive' parameters type */
export interface IDeleteEventRecursiveParams {
  id: NumberOrString;
}

/** 'DeleteEventRecursive' return type */
export interface IDeleteEventRecursiveResult {
  deleted: boolean;
}

/** 'DeleteEventRecursive' query type */
export interface IDeleteEventRecursiveQuery {
  params: IDeleteEventRecursiveParams;
  result: IDeleteEventRecursiveResult;
}

const deleteEventRecursiveIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":59,"b":62},{"a":293,"b":296}]}],"statement":"with recursive tree as (\n  select id from event where id = :id!\n  union all\n  select child.id from event child join tree parent on child.parent_id = parent.id\n), deleted as (\n  delete from event where id in (select id from tree)\n  returning id\n)\nselect exists(select 1 from deleted where id = :id!) as \"deleted!\""};

/**
 * Query generated from SQL:
 * ```
 * with recursive tree as (
 *   select id from event where id = :id!
 *   union all
 *   select child.id from event child join tree parent on child.parent_id = parent.id
 * ), deleted as (
 *   delete from event where id in (select id from tree)
 *   returning id
 * )
 * select exists(select 1 from deleted where id = :id!) as "deleted!"
 * ```
 */
export const deleteEventRecursive = new PreparedQuery<IDeleteEventRecursiveParams,IDeleteEventRecursiveResult>(deleteEventRecursiveIR);


