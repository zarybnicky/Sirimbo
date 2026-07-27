/* @name SharedEventInstance */
select
  instance.id::text as "id!",
  instance.name as "name",
  instance.type::text as "type",
  instance.since::text as "since!",
  instance.until::text as "until!",
  instance.is_cancelled as "isCancelled!",
  instance.location_id::text as "locationId",
  location.name as "locationName",
  instance.location_text as "locationText",
  instance.summary as "summary",
  instance.description as "description",
  instance.capacity as "capacity",
  instance.capacity_unit::text as "capacityUnit!",
  event_instance_remaining_person_spots(instance) as "remainingPersonSpots",
  instance.has_public_details as "hasPublicDetails!",
  (
    :shareToken::text is not null
    and instance.share_token = :shareToken::text
  ) as "hasTokenAccess!",
  case
    when instance.has_public_details or instance.share_token = :shareToken::text then
      array(
        select person.name
        from event_instance_trainer trainer
        join person person on person.id = trainer.person_id
        where trainer.instance_id = instance.id
        order by trainer.id
      )
    else '{}'::text[]
  end as "trainerNames!"
from event_instance instance
left join tenant_location location on location.id = instance.location_id
where instance.id = :id!::bigint
  and instance.tenant_id = :tenantId!::bigint
  and (instance.is_public is true or instance.share_token = :shareToken::text);
