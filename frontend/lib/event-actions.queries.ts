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

const cloneCampEventIR: any = {"usedParamSet":{"parentId":true,"shift":true,"copyTrainers":true,"copyCohorts":true,"sourceId":true},"params":[{"name":"parentId","required":false,"transform":{"type":"scalar"},"locs":[{"a":211,"b":219}]},{"name":"shift","required":true,"transform":{"type":"scalar"},"locs":[{"a":437,"b":443},{"a":481,"b":487}]},{"name":"copyTrainers","required":true,"transform":{"type":"scalar"},"locs":[{"a":593,"b":606}]},{"name":"copyCohorts","required":true,"transform":{"type":"scalar"},"locs":[{"a":811,"b":823}]},{"name":"sourceId","required":true,"transform":{"type":"scalar"},"locs":[{"a":973,"b":982}]}],"statement":"select saved.id::text as \"id!\"\nfrom event source\ncross join lateral save_events(\n  details => jsonb_populate_record(\n    null::event_details_input,\n    to_jsonb(source) || jsonb_build_object(\n      'parent_id', :parentId::bigint,\n      'name', case when source.type = 'lesson' then null else source.name end,\n      'capacity', coalesce(source.capacity, 0),\n      'is_locked', null\n    )),\n  events => array[\n    row(null, source.since + :shift!::text::interval,\n    source.until + :shift!::text::interval,\n    false, '{}'::event_registration_input[])::event_input\n  ],\n  trainers => case when :copyTrainers!::boolean then array(\n    select row(person_id, lessons_offered)::event_trainer_input\n    from event_trainer where event_id = source.id\n  ) else '{}'::event_trainer_input[] end,\n  cohort_ids => case when :copyCohorts!::boolean then array(\n    select cohort_id from event_target_cohort where event_id = source.id\n  ) else '{}'::bigint[] end\n) saved\nwhere source.id = :sourceId!"};

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
 *       'is_locked', null
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
  copyId: NumberOrString;
  sourceId: NumberOrString;
}

/** 'CopyEventMetadata' return type */
export type ICopyEventMetadataResult = void;

/** 'CopyEventMetadata' query type */
export interface ICopyEventMetadataQuery {
  params: ICopyEventMetadataParams;
  result: ICopyEventMetadataResult;
}

const copyEventMetadataIR: any = {"usedParamSet":{"copyId":true,"sourceId":true},"params":[{"name":"copyId","required":true,"transform":{"type":"scalar"},"locs":[{"a":163,"b":170}]},{"name":"sourceId","required":true,"transform":{"type":"scalar"},"locs":[{"a":188,"b":197}]}],"statement":"update event copy\nset summary = source.summary,\n    description = source.description,\n    cover_image_id = source.cover_image_id\nfrom event source where copy.id = :copyId! and source.id = :sourceId!"};

/**
 * Query generated from SQL:
 * ```
 * update event copy
 * set summary = source.summary,
 *     description = source.description,
 *     cover_image_id = source.cover_image_id
 * from event source where copy.id = :copyId! and source.id = :sourceId!
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
  id?: NumberOrString | null | void;
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

const eventSeriesEventsIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":80,"b":82}]}],"statement":"select i.id from event_series s join event i on s.id = i.series_id where s.id = :id"};

/**
 * Query generated from SQL:
 * ```
 * select i.id from event_series s join event i on s.id = i.series_id where s.id = :id
 * ```
 */
export const eventSeriesEvents = new PreparedQuery<IEventSeriesEventsParams,IEventSeriesEventsResult>(eventSeriesEventsIR);


/** 'DeleteEventRecursive' parameters type */
export interface IDeleteEventRecursiveParams {
  id: NumberOrString;
}

/** 'DeleteEventRecursive' return type */
export interface IDeleteEventRecursiveResult {
  id: string;
}

/** 'DeleteEventRecursive' query type */
export interface IDeleteEventRecursiveQuery {
  params: IDeleteEventRecursiveParams;
  result: IDeleteEventRecursiveResult;
}

const deleteEventRecursiveIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":59,"b":62}]}],"statement":"with recursive tree as (\n  select id from event where id = :id!\n  union all\n  select child.id from event child join tree parent on child.parent_id = parent.id\n)\ndelete from event where id in (select id from tree)\nreturning id::text as \"id!\""};

/**
 * Query generated from SQL:
 * ```
 * with recursive tree as (
 *   select id from event where id = :id!
 *   union all
 *   select child.id from event child join tree parent on child.parent_id = parent.id
 * )
 * delete from event where id in (select id from tree)
 * returning id::text as "id!"
 * ```
 */
export const deleteEventRecursive = new PreparedQuery<IDeleteEventRecursiveParams,IDeleteEventRecursiveResult>(deleteEventRecursiveIR);


