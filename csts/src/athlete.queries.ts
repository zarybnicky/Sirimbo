/** Types generated for queries found in "src/athlete.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type DateOrString = Date | string;

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

/** 'TouchIngestRecord' parameters type */
export interface ITouchIngestRecordParams {
  hash?: string | null | void;
  type?: string | null | void;
  url?: string | null | void;
}

/** 'TouchIngestRecord' return type */
export type ITouchIngestRecordResult = void;

/** 'TouchIngestRecord' query type */
export interface ITouchIngestRecordQuery {
  params: ITouchIngestRecordParams;
  result: ITouchIngestRecordResult;
}

const touchIngestRecordIR: any = {"usedParamSet":{"type":true,"url":true,"hash":true},"params":[{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":146,"b":150}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":162,"b":165}]},{"name":"hash","required":false,"transform":{"type":"scalar"},"locs":[{"a":178,"b":182}]}],"statement":"update csts.ingest\nset\n  checked_at = now(),\n  checked_without_change = coalesce(checked_without_change, 0) + 1,\n  last_error = null\nwhere type = :type and url = :url and hash = :hash"};

/**
 * Query generated from SQL:
 * ```
 * update csts.ingest
 * set
 *   checked_at = now(),
 *   checked_without_change = coalesce(checked_without_change, 0) + 1,
 *   last_error = null
 * where type = :type and url = :url and hash = :hash
 * ```
 */
export const touchIngestRecord = new PreparedQuery<ITouchIngestRecordParams,ITouchIngestRecordResult>(touchIngestRecordIR);


/** 'UpsertIngestRecord' parameters type */
export interface IUpsertIngestRecordParams {
  hash?: string | null | void;
  payload?: Json | null | void;
  type?: string | null | void;
  url?: string | null | void;
}

/** 'UpsertIngestRecord' return type */
export type IUpsertIngestRecordResult = void;

/** 'UpsertIngestRecord' query type */
export interface IUpsertIngestRecordQuery {
  params: IUpsertIngestRecordParams;
  result: IUpsertIngestRecordResult;
}

const upsertIngestRecordIR: any = {"usedParamSet":{"type":true,"url":true,"hash":true,"payload":true},"params":[{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":95,"b":99}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":102,"b":105}]},{"name":"hash","required":false,"transform":{"type":"scalar"},"locs":[{"a":108,"b":112}]},{"name":"payload","required":false,"transform":{"type":"scalar"},"locs":[{"a":115,"b":122}]}],"statement":"insert into csts.ingest (type, url, hash, payload, last_error, checked_without_change)\nvalues (:type, :url, :hash, :payload::jsonb, null, 0)\non conflict (type, url, hash)\ndo update set\n  payload = excluded.payload,\n  checked_at = now(),\n  last_error = null,\n  checked_without_change = 0"};

/**
 * Query generated from SQL:
 * ```
 * insert into csts.ingest (type, url, hash, payload, last_error, checked_without_change)
 * values (:type, :url, :hash, :payload::jsonb, null, 0)
 * on conflict (type, url, hash)
 * do update set
 *   payload = excluded.payload,
 *   checked_at = now(),
 *   last_error = null,
 *   checked_without_change = 0
 * ```
 */
export const upsertIngestRecord = new PreparedQuery<IUpsertIngestRecordParams,IUpsertIngestRecordResult>(upsertIngestRecordIR);


/** 'LoadIngestRecord' parameters type */
export interface ILoadIngestRecordParams {
  type?: string | null | void;
  url?: string | null | void;
}

/** 'LoadIngestRecord' return type */
export interface ILoadIngestRecordResult {
  hash: string;
  payload: Json;
  created_at: Date;
  checked_at: Date;
  last_error: string | null;
  checked_without_change: number;
}

/** 'LoadIngestRecord' query type */
export interface ILoadIngestRecordQuery {
  params: ILoadIngestRecordParams;
  result: ILoadIngestRecordResult;
}

const loadIngestRecordIR: any = {"usedParamSet":{"type":true,"url":true},"params":[{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":111,"b":115}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":127,"b":130}]}],"statement":"select hash, payload, created_at, checked_at, last_error, checked_without_change\nfrom csts.ingest\nwhere type = :type and url = :url\norder by checked_at desc, created_at desc\nlimit 1"};

/**
 * Query generated from SQL:
 * ```
 * select hash, payload, created_at, checked_at, last_error, checked_without_change
* from csts.ingest
* where type = :type and url = :url
 * order by checked_at desc, created_at desc
 * limit 1
 * ```
 */
export const loadIngestRecord = new PreparedQuery<ILoadIngestRecordParams,ILoadIngestRecordResult>(loadIngestRecordIR);


/** 'UpdateLatestIngestError' parameters type */
export interface IUpdateLatestIngestErrorParams {
  hash?: string | null | void;
  lastError?: string | null | void;
  type?: string | null | void;
  url?: string | null | void;
}

/** 'UpdateLatestIngestError' return type */
export type IUpdateLatestIngestErrorResult = void;

/** 'UpdateLatestIngestError' query type */
export interface IUpdateLatestIngestErrorQuery {
  params: IUpdateLatestIngestErrorParams;
  result: IUpdateLatestIngestErrorResult;
}

const updateLatestIngestErrorIR: any = {"usedParamSet":{"type":true,"url":true,"lastError":true,"hash":true},"params":[{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":65,"b":69},{"a":252,"b":256}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":81,"b":84},{"a":277,"b":280}]},{"name":"lastError","required":false,"transform":{"type":"scalar"},"locs":[{"a":185,"b":194}]},{"name":"hash","required":false,"transform":{"type":"scalar"},"locs":[{"a":311,"b":315}]}],"statement":"with latest as (\n  select hash\n  from csts.ingest\n  where type = :type and url = :url\n  order by checked_at desc, created_at desc\n  limit 1\n)\nupdate csts.ingest target\nset last_error = :lastError,\n    checked_at = now()\nfrom latest\nwhere target.type = :type\n  and target.url = :url\n  and target.hash = coalesce(:hash, latest.hash);"};

/**
 * Query generated from SQL:
 * ```
 * with latest as (
 *   select hash
 *   from csts.ingest
 *   where type = :type and url = :url
 *   order by checked_at desc, created_at desc
 *   limit 1
 * )
 * update csts.ingest target
 * set last_error = :lastError,
 *     checked_at = now()
 * from latest
 * where target.type = :type
 *   and target.url = :url
 *   and target.hash = coalesce(:hash, latest.hash);
 * ```
 */
export const updateLatestIngestError = new PreparedQuery<IUpdateLatestIngestErrorParams,IUpdateLatestIngestErrorResult>(updateLatestIngestErrorIR);


/** 'DeleteIngestByUrls' parameters type */
export interface IDeleteIngestByUrlsParams {
  type?: string | null | void;
  urls: readonly (string | null | void)[];
}

/** 'DeleteIngestByUrls' return type */
export type IDeleteIngestByUrlsResult = void;

/** 'DeleteIngestByUrls' query type */
export interface IDeleteIngestByUrlsQuery {
  params: IDeleteIngestByUrlsParams;
  result: IDeleteIngestByUrlsResult;
}

const deleteIngestByUrlsIR: any = {"usedParamSet":{"type":true,"urls":true},"params":[{"name":"urls","required":false,"transform":{"type":"array_spread"},"locs":[{"a":54,"b":58}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":37,"b":41}]}],"statement":"delete from csts.ingest where type = :type and url in :urls"};

/**
 * Query generated from SQL:
 * ```
 * delete from csts.ingest where type = :type and url in :urls
 * ```
 */
export const deleteIngestByUrls = new PreparedQuery<IDeleteIngestByUrlsParams,IDeleteIngestByUrlsResult>(deleteIngestByUrlsIR);


/** 'SelectMaxAthleteIdt' return type */
export interface ISelectMaxAthleteIdtResult {
  max: number | null;
}

/** 'SelectMaxAthleteIdt' query type */
export interface ISelectMaxAthleteIdtQuery {
  params: void;
  result: ISelectMaxAthleteIdtResult;
}

const selectMaxAthleteIdtIR: any = {"usedParamSet":{},"params":[],"statement":"select idt as max\nfrom csts.athlete\norder by case\n  when idt between 18000000 and 18092599 then 1\n  when idt between 10600000 and 17999999 then 2\n  when idt between 18095000 and 19999000 then 3\n  else 0\nend desc,\n  idt desc\nlimit 1"};

/**
 * Query generated from SQL:
 * ```
 * select idt as max
 * from csts.athlete
 * order by case
 *   when idt between 18000000 and 18092599 then 1
 *   when idt between 10600000 and 17999999 then 2
 *   when idt between 18095000 and 19999000 then 3
 *   else 0
 * end desc,
 *   idt desc
 * limit 1
 * ```
 */
export const selectMaxAthleteIdt = new PreparedQuery<void,ISelectMaxAthleteIdtResult>(selectMaxAthleteIdtIR);


/** 'SelectAthletesToRefresh' parameters type */
export interface ISelectAthletesToRefreshParams {
  limit?: number | null | void;
  threshold?: DateOrString | null | void;
}

/** 'SelectAthletesToRefresh' return type */
export interface ISelectAthletesToRefreshResult {
  idt: number;
}

/** 'SelectAthletesToRefresh' query type */
export interface ISelectAthletesToRefreshQuery {
  params: ISelectAthletesToRefreshParams;
  result: ISelectAthletesToRefreshResult;
}

const selectAthletesToRefreshIR: any = {"usedParamSet":{"threshold":true,"limit":true},"params":[{"name":"threshold","required":false,"transform":{"type":"scalar"},"locs":[{"a":74,"b":83}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":143,"b":148}]}],"statement":"select idt\nfrom csts.athlete\nwhere last_checked is null or last_checked < :threshold::timestamptz\norder by last_checked nulls first, idt\nlimit :limit"};

/**
 * Query generated from SQL:
 * ```
 * select idt
 * from csts.athlete
 * where last_checked is null or last_checked < :threshold::timestamptz
 * order by last_checked nulls first, idt
 * limit :limit
 * ```
 */
export const selectAthletesToRefresh = new PreparedQuery<ISelectAthletesToRefreshParams,ISelectAthletesToRefreshResult>(selectAthletesToRefreshIR);


/** 'UpdateAthleteLastChecked' parameters type */
export interface IUpdateAthleteLastCheckedParams {
  idt?: number | null | void;
}

/** 'UpdateAthleteLastChecked' return type */
export type IUpdateAthleteLastCheckedResult = void;

/** 'UpdateAthleteLastChecked' query type */
export interface IUpdateAthleteLastCheckedQuery {
  params: IUpdateAthleteLastCheckedParams;
  result: IUpdateAthleteLastCheckedResult;
}

const updateAthleteLastCheckedIR: any = {"usedParamSet":{"idt":true},"params":[{"name":"idt","required":false,"transform":{"type":"scalar"},"locs":[{"a":57,"b":60}]}],"statement":"update csts.athlete\nset last_checked = now()\nwhere idt = :idt"};

/**
 * Query generated from SQL:
 * ```
 * update csts.athlete
 * set last_checked = now()
 * where idt = :idt
 * ```
 */
export const updateAthleteLastChecked = new PreparedQuery<IUpdateAthleteLastCheckedParams,IUpdateAthleteLastCheckedResult>(updateAthleteLastCheckedIR);


/** 'UpsertAthlete' parameters type */
export interface IUpsertAthleteParams {
  age?: string | null | void;
  idt?: number | null | void;
  medicalCheckupExpiration?: DateOrString | null | void;
  name?: string | null | void;
  sex?: string | null | void;
}

/** 'UpsertAthlete' return type */
export type IUpsertAthleteResult = void;

/** 'UpsertAthlete' query type */
export interface IUpsertAthleteQuery {
  params: IUpsertAthleteParams;
  result: IUpsertAthleteResult;
}

const upsertAthleteIR: any = {"usedParamSet":{"idt":true,"name":true,"age":true,"sex":true,"medicalCheckupExpiration":true},"params":[{"name":"idt","required":false,"transform":{"type":"scalar"},"locs":[{"a":104,"b":107}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":110,"b":114}]},{"name":"age","required":false,"transform":{"type":"scalar"},"locs":[{"a":117,"b":120}]},{"name":"sex","required":false,"transform":{"type":"scalar"},"locs":[{"a":129,"b":132}]},{"name":"medicalCheckupExpiration","required":false,"transform":{"type":"scalar"},"locs":[{"a":135,"b":159}]}],"statement":"insert into csts.athlete (\n  idt,\n  name,\n  age_category,\n  sex,\n  medical_checkup_expiration\n)\nvalues (:idt, :name, :age::text, :sex, :medicalCheckupExpiration::date)\non conflict (idt) do update set\n  name = excluded.name,\n  age_category = excluded.age_category,\n  sex = excluded.sex,\n  medical_checkup_expiration = excluded.medical_checkup_expiration,\n  fetched_at = now()"};

/**
 * Query generated from SQL:
 * ```
 * insert into csts.athlete (
 *   idt,
 *   name,
 *   age_category,
 *   sex,
 *   medical_checkup_expiration
 * )
 * values (:idt, :name, :age::text, :sex, :medicalCheckupExpiration::date)
 * on conflict (idt) do update set
 *   name = excluded.name,
 *   age_category = excluded.age_category,
 *   sex = excluded.sex,
 *   medical_checkup_expiration = excluded.medical_checkup_expiration,
 *   fetched_at = now()
 * ```
 */
export const upsertAthlete = new PreparedQuery<IUpsertAthleteParams,IUpsertAthleteResult>(upsertAthleteIR);


/** 'UpsertCompetitorRanking' parameters type */
export interface IUpsertCompetitorRankingParams {
  age?: string | null | void;
  athleteIdt?: number | null | void;
  class?: string | null | void;
  competitorId?: number | null | void;
  competitors?: string | null | void;
  coupleId?: number | null | void;
  discipline?: string | null | void;
  domesticFinaleCount?: number | null | void;
  foreignFinaleCount?: number | null | void;
  points?: number | null | void;
  rankingAge?: string | null | void;
  rankingPointsAge?: string | null | void;
  ranklistPoints?: number | null | void;
  ranklistRanking?: number | null | void;
  series?: string | null | void;
}

/** 'UpsertCompetitorRanking' return type */
export type IUpsertCompetitorRankingResult = void;

/** 'UpsertCompetitorRanking' query type */
export interface IUpsertCompetitorRankingQuery {
  params: IUpsertCompetitorRankingParams;
  result: IUpsertCompetitorRankingResult;
}

const upsertCompetitorRankingIR: any = {"usedParamSet":{"competitorId":true,"discipline":true,"rankingPointsAge":true,"rankingAge":true,"age":true,"series":true,"competitors":true,"class":true,"points":true,"domesticFinaleCount":true,"foreignFinaleCount":true,"ranklistRanking":true,"ranklistPoints":true,"athleteIdt":true,"coupleId":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":296,"b":308}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":311,"b":321}]},{"name":"rankingPointsAge","required":false,"transform":{"type":"scalar"},"locs":[{"a":324,"b":340}]},{"name":"rankingAge","required":false,"transform":{"type":"scalar"},"locs":[{"a":343,"b":353}]},{"name":"age","required":false,"transform":{"type":"scalar"},"locs":[{"a":356,"b":359}]},{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":362,"b":368}]},{"name":"competitors","required":false,"transform":{"type":"scalar"},"locs":[{"a":371,"b":382}]},{"name":"class","required":false,"transform":{"type":"scalar"},"locs":[{"a":385,"b":390}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":393,"b":399}]},{"name":"domesticFinaleCount","required":false,"transform":{"type":"scalar"},"locs":[{"a":404,"b":423}]},{"name":"foreignFinaleCount","required":false,"transform":{"type":"scalar"},"locs":[{"a":426,"b":444}]},{"name":"ranklistRanking","required":false,"transform":{"type":"scalar"},"locs":[{"a":447,"b":462}]},{"name":"ranklistPoints","required":false,"transform":{"type":"scalar"},"locs":[{"a":465,"b":479}]},{"name":"athleteIdt","required":false,"transform":{"type":"scalar"},"locs":[{"a":482,"b":492}]},{"name":"coupleId","required":false,"transform":{"type":"scalar"},"locs":[{"a":495,"b":503}]}],"statement":"insert into csts.competitor_ranking (\n  competitor_id,\n  discipline,\n  ranking_points_age,\n  ranking_age,\n  competitor_age,\n  series,\n  competitors,\n  class,\n  points,\n  domestic_finale_count,\n  foreign_finale_count,\n  ranklist_ranking,\n  ranklist_points,\n  athlete_idt,\n  couple_id\n)\nvalues (\n  :competitorId, :discipline, :rankingPointsAge, :rankingAge, :age, :series, :competitors, :class, :points,\n  :domesticFinaleCount, :foreignFinaleCount, :ranklistRanking, :ranklistPoints, :athleteIdt, :coupleId\n)\non conflict (competitor_id, discipline) do update set\n  ranking_points_age = excluded.ranking_points_age,\n  ranking_age = excluded.ranking_age,\n  competitor_age = excluded.competitor_age,\n  series = excluded.series,\n  competitors = excluded.competitors,\n  class = excluded.class,\n  points = excluded.points,\n  domestic_finale_count = excluded.domestic_finale_count,\n  foreign_finale_count = excluded.foreign_finale_count,\n  ranklist_ranking = excluded.ranklist_ranking,\n  ranklist_points = excluded.ranklist_points,\n  athlete_idt = excluded.athlete_idt,\n  couple_id = excluded.couple_id"};

/**
 * Query generated from SQL:
 * ```
 * insert into csts.competitor_ranking (
 *   competitor_id,
 *   discipline,
 *   ranking_points_age,
 *   ranking_age,
 *   competitor_age,
 *   series,
 *   competitors,
 *   class,
 *   points,
 *   domestic_finale_count,
 *   foreign_finale_count,
 *   ranklist_ranking,
 *   ranklist_points,
 *   athlete_idt,
 *   couple_id
 * )
 * values (
 *   :competitorId, :discipline, :rankingPointsAge, :rankingAge, :age, :series, :competitors, :class, :points,
 *   :domesticFinaleCount, :foreignFinaleCount, :ranklistRanking, :ranklistPoints, :athleteIdt, :coupleId
 * )
 * on conflict (competitor_id, discipline) do update set
 *   ranking_points_age = excluded.ranking_points_age,
 *   ranking_age = excluded.ranking_age,
 *   competitor_age = excluded.competitor_age,
 *   series = excluded.series,
 *   competitors = excluded.competitors,
 *   class = excluded.class,
 *   points = excluded.points,
 *   domestic_finale_count = excluded.domestic_finale_count,
 *   foreign_finale_count = excluded.foreign_finale_count,
 *   ranklist_ranking = excluded.ranklist_ranking,
 *   ranklist_points = excluded.ranklist_points,
 *   athlete_idt = excluded.athlete_idt,
 *   couple_id = excluded.couple_id
 * ```
 */
export const upsertCompetitorRanking = new PreparedQuery<IUpsertCompetitorRankingParams,IUpsertCompetitorRankingResult>(upsertCompetitorRankingIR);


/** 'UpsertAthleteRanking' parameters type */
export interface IUpsertAthleteRankingParams {
  athleteId?: number | null | void;
  discipline?: string | null | void;
  personalClass?: string | null | void;
  personalDomesticFinaleCount?: number | null | void;
  personalForeignFinaleCount?: number | null | void;
  personalPoints?: number | null | void;
  series?: string | null | void;
}

/** 'UpsertAthleteRanking' return type */
export type IUpsertAthleteRankingResult = void;

/** 'UpsertAthleteRanking' query type */
export interface IUpsertAthleteRankingQuery {
  params: IUpsertAthleteRankingParams;
  result: IUpsertAthleteRankingResult;
}

const upsertAthleteRankingIR: any = {"usedParamSet":{"athleteId":true,"discipline":true,"series":true,"personalClass":true,"personalPoints":true,"personalDomesticFinaleCount":true,"personalForeignFinaleCount":true},"params":[{"name":"athleteId","required":false,"transform":{"type":"scalar"},"locs":[{"a":186,"b":195}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":198,"b":208}]},{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":211,"b":217}]},{"name":"personalClass","required":false,"transform":{"type":"scalar"},"locs":[{"a":220,"b":233}]},{"name":"personalPoints","required":false,"transform":{"type":"scalar"},"locs":[{"a":236,"b":250}]},{"name":"personalDomesticFinaleCount","required":false,"transform":{"type":"scalar"},"locs":[{"a":253,"b":280}]},{"name":"personalForeignFinaleCount","required":false,"transform":{"type":"scalar"},"locs":[{"a":283,"b":309}]}],"statement":"insert into csts.athlete_ranking (\n  athlete_id,\n  discipline,\n  series,\n  personal_class,\n  personal_points,\n  personal_domestic_finale_count,\n  personal_foreign_finale_count\n)\nvalues (:athleteId, :discipline, :series, :personalClass, :personalPoints, :personalDomesticFinaleCount, :personalForeignFinaleCount)\non conflict (athlete_id, discipline, series) do update set\n  personal_class = excluded.personal_class,\n  personal_points = excluded.personal_points,\n  personal_domestic_finale_count = excluded.personal_domestic_finale_count,\n  personal_foreign_finale_count = excluded.personal_foreign_finale_count"};

/**
 * Query generated from SQL:
 * ```
 * insert into csts.athlete_ranking (
 *   athlete_id,
 *   discipline,
 *   series,
 *   personal_class,
 *   personal_points,
 *   personal_domestic_finale_count,
 *   personal_foreign_finale_count
 * )
 * values (:athleteId, :discipline, :series, :personalClass, :personalPoints, :personalDomesticFinaleCount, :personalForeignFinaleCount)
 * on conflict (athlete_id, discipline, series) do update set
 *   personal_class = excluded.personal_class,
 *   personal_points = excluded.personal_points,
 *   personal_domestic_finale_count = excluded.personal_domestic_finale_count,
 *   personal_foreign_finale_count = excluded.personal_foreign_finale_count
 * ```
 */
export const upsertAthleteRanking = new PreparedQuery<IUpsertAthleteRankingParams,IUpsertAthleteRankingResult>(upsertAthleteRankingIR);


/** 'UpsertCouple' parameters type */
export interface IUpsertCoupleParams {
  athleteIdt?: number | null | void;
  competitorId?: number | null | void;
  coupleIdt?: number | null | void;
  formedAt?: DateOrString | null | void;
  partnerIdt?: number | null | void;
}

/** 'UpsertCouple' return type */
export interface IUpsertCoupleResult {
  id: number;
}

/** 'UpsertCouple' query type */
export interface IUpsertCoupleQuery {
  params: IUpsertCoupleParams;
  result: IUpsertCoupleResult;
}

const upsertCoupleIR: any = {"usedParamSet":{"competitorId":true,"coupleIdt":true,"formedAt":true,"partnerIdt":true,"athleteIdt":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":81,"b":93}]},{"name":"coupleIdt","required":false,"transform":{"type":"scalar"},"locs":[{"a":98,"b":107}]},{"name":"formedAt","required":false,"transform":{"type":"scalar"},"locs":[{"a":244,"b":252}]},{"name":"partnerIdt","required":false,"transform":{"type":"scalar"},"locs":[{"a":321,"b":331}]},{"name":"athleteIdt","required":false,"transform":{"type":"scalar"},"locs":[{"a":348,"b":358}]}],"statement":"insert into csts.couple (id, couple_idt, man_idt, woman_idt, formed_at)\nselect\n  :competitorId,\n  :coupleIdt,\n  case when a1.sex = 'M' then a1.idt else a2.idt end as man_idt,\n  case when a1.sex = 'F' then a1.idt else a2.idt end as woman_idt,\n  :formedAt::timestamptz\nfrom csts.athlete a1\njoin csts.athlete a2 on a2.idt = :partnerIdt\nwhere a1.idt = :athleteIdt\n  and ((a1.sex = 'M' and a2.sex = 'F') or (a1.sex = 'F' and a2.sex = 'M'))\non conflict (id) do update set\n  couple_idt = excluded.couple_idt,\n  man_idt    = excluded.man_idt,\n  woman_idt  = excluded.woman_idt,\n  formed_at  = excluded.formed_at\nreturning id"};

/**
 * Query generated from SQL:
 * ```
 * insert into csts.couple (id, couple_idt, man_idt, woman_idt, formed_at)
 * select
 *   :competitorId,
 *   :coupleIdt,
 *   case when a1.sex = 'M' then a1.idt else a2.idt end as man_idt,
 *   case when a1.sex = 'F' then a1.idt else a2.idt end as woman_idt,
 *   :formedAt::timestamptz
 * from csts.athlete a1
 * join csts.athlete a2 on a2.idt = :partnerIdt
 * where a1.idt = :athleteIdt
 *   and ((a1.sex = 'M' and a2.sex = 'F') or (a1.sex = 'F' and a2.sex = 'M'))
 * on conflict (id) do update set
 *   couple_idt = excluded.couple_idt,
 *   man_idt    = excluded.man_idt,
 *   woman_idt  = excluded.woman_idt,
 *   formed_at  = excluded.formed_at
 * returning id
 * ```
 */
export const upsertCouple = new PreparedQuery<IUpsertCoupleParams,IUpsertCoupleResult>(upsertCoupleIR);


