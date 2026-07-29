/** Types generated for queries found in "app/(member)/termin/[id]/termin.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type NumberOrString = number | string;

export type stringArray = (string)[];

/** 'SharedEventInstance' parameters type */
export interface ISharedEventInstanceParams {
  id: NumberOrString;
  shareToken?: string | null | void;
  tenantId: NumberOrString;
}

/** 'SharedEventInstance' return type */
export interface ISharedEventInstanceResult {
  capacity: number | null;
  capacityUnit: string;
  description: string | null;
  hasPublicDetails: boolean;
  hasTokenAccess: boolean;
  id: string;
  isCancelled: boolean;
  locationId: string | null;
  locationName: string;
  locationText: string | null;
  name: string | null;
  remainingPersonSpots: number | null;
  since: string;
  summary: string | null;
  trainerNames: stringArray;
  type: string | null;
  until: string;
}

/** 'SharedEventInstance' query type */
export interface ISharedEventInstanceQuery {
  params: ISharedEventInstanceParams;
  result: ISharedEventInstanceResult;
}

const sharedEventInstanceIR: any = {"usedParamSet":{"shareToken":true,"id":true,"tenantId":true},"params":[{"name":"shareToken","required":false,"transform":{"type":"scalar"},"locs":[{"a":636,"b":646},{"a":697,"b":707},{"a":811,"b":821},{"a":1350,"b":1360}]},{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":1231,"b":1234}]},{"name":"tenantId","required":true,"transform":{"type":"scalar"},"locs":[{"a":1271,"b":1280}]}],"statement":"select\n  instance.id::text as \"id!\",\n  instance.name as \"name\",\n  instance.type::text as \"type\",\n  instance.since::text as \"since!\",\n  instance.until::text as \"until!\",\n  instance.is_cancelled as \"isCancelled!\",\n  instance.location_id::text as \"locationId\",\n  location.name as \"locationName\",\n  instance.location_text as \"locationText\",\n  instance.summary as \"summary\",\n  instance.description as \"description\",\n  instance.capacity as \"capacity\",\n  instance.capacity_unit::text as \"capacityUnit!\",\n  event_instance_remaining_person_spots(instance) as \"remainingPersonSpots\",\n  instance.has_public_details as \"hasPublicDetails!\",\n  (\n    :shareToken::text is not null\n    and instance.share_token = :shareToken::text\n  ) as \"hasTokenAccess!\",\n  case\n    when instance.has_public_details or instance.share_token = :shareToken::text then\n      array(\n        select person.name\n        from event_instance_trainer trainer\n        join person person on person.id = trainer.person_id\n        where trainer.instance_id = instance.id\n        order by trainer.id\n      )\n    else '{}'::text[]\n  end as \"trainerNames!\"\nfrom event_instance instance\nleft join tenant_location location on location.id = instance.location_id\nwhere instance.id = :id!::bigint\n  and instance.tenant_id = :tenantId!::bigint\n  and (instance.is_public is true or instance.share_token = :shareToken::text)"};

/**
 * Query generated from SQL:
 * ```
 * select
 *   instance.id::text as "id!",
 *   instance.name as "name",
 *   instance.type::text as "type",
 *   instance.since::text as "since!",
 *   instance.until::text as "until!",
 *   instance.is_cancelled as "isCancelled!",
 *   instance.location_id::text as "locationId",
 *   location.name as "locationName",
 *   instance.location_text as "locationText",
 *   instance.summary as "summary",
 *   instance.description as "description",
 *   instance.capacity as "capacity",
 *   instance.capacity_unit::text as "capacityUnit!",
 *   event_instance_remaining_person_spots(instance) as "remainingPersonSpots",
 *   instance.has_public_details as "hasPublicDetails!",
 *   (
 *     :shareToken::text is not null
 *     and instance.share_token = :shareToken::text
 *   ) as "hasTokenAccess!",
 *   case
 *     when instance.has_public_details or instance.share_token = :shareToken::text then
 *       array(
 *         select person.name
 *         from event_instance_trainer trainer
 *         join person person on person.id = trainer.person_id
 *         where trainer.instance_id = instance.id
 *         order by trainer.id
 *       )
 *     else '{}'::text[]
 *   end as "trainerNames!"
 * from event_instance instance
 * left join tenant_location location on location.id = instance.location_id
 * where instance.id = :id!::bigint
 *   and instance.tenant_id = :tenantId!::bigint
 *   and (instance.is_public is true or instance.share_token = :shareToken::text)
 * ```
 */
export const sharedEventInstance = new PreparedQuery<ISharedEventInstanceParams,ISharedEventInstanceResult>(sharedEventInstanceIR);


