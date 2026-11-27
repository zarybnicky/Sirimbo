import type { Resource } from '@/calendar/types';
import type { EventFormOptionsQuery, EventWithTrainerFragment } from '@/graphql/Event';
import { EventFormSchema, type EventFormType } from '@/ui/event-form/types';
import { addMinutes, isSameDay } from 'date-fns';

export type CreateEventDefaults = {
  date?: Date;
  after?: EventWithTrainerFragment;
  parent?: EventWithTrainerFragment;
  events?: readonly EventWithTrainerFragment[];
  type?: EventFormType['type'];
  since?: Date;
  until?: Date;
  locationId?: string | null;
  locationText?: string;
};

export function splitIntoLessonRanges(since: Date, until: Date) {
  const ranges: { since: Date; until: Date }[] = [];
  let start = since;

  while (addMinutes(start, 45) <= until) {
    const end = addMinutes(start, 45);
    ranges.push({ since: start, until: end });
    start = end;
  }
  return ranges;
}

export function defaultsFromSlot(slot: {
  start: Date;
  end: Date;
  action: 'select' | 'click' | 'day';
  resource?: Resource;
}) {
  if (slot.action === 'day') return { date: slot.start };

  const { resource } = slot;
  return {
    since: slot.start,
    until: slot.action === 'select' ? slot.end : undefined,
    trainerPersonIds:
      resource?.type === 'person'
        ? [resource.id]
        : resource?.type === 'unassigned'
          ? []
          : undefined,
    locationId: resource?.type === 'location' ? resource.id : undefined,
    locationText: resource?.type === 'locationText' ? resource.id : undefined,
  };
}

export function createEventDefaults(
  input: CreateEventDefaults,
  fallback: {
    lockEventsByDefault: boolean;
    trainerId?: string;
    allowedTrainerIds: readonly string[];
  },
): EventFormType {
  const { after, events = [], type = 'LESSON' } = input;
  const since = new Date(input.since ?? after?.until ?? input.date ?? new Date());
  if (!input.since && !after) since.setHours(type === 'CAMP' ? 9 : 15, 0, 0, 0);
  const until = input.until ?? addMinutes(since, type === 'CAMP' ? 480 : 45);
  const prefilledTrainerIds =
    after?.eventTrainersList.map((x) => x.personId) ??
    (fallback.trainerId ? [fallback.trainerId] : []);
  const trainerPersonIds = prefilledTrainerIds.filter((id) =>
    fallback.allowedTrainerIds.includes(id),
  );
  if (
    prefilledTrainerIds.length > 0 &&
    trainerPersonIds.length === 0 &&
    fallback.trainerId
  ) {
    trainerPersonIds.push(fallback.trainerId);
  }

  let previous = after;
  if (!previous) {
    for (const event of events) {
      if (
        !event.isCancelled &&
        isSameDay(new Date(event.since), since) &&
        new Date(event.until) <= since &&
        event.eventTrainersList.some((x) => trainerPersonIds.includes(x.personId)) &&
        (!previous || new Date(event.since) > new Date(previous.since))
      ) {
        previous = event;
      }
    }
  }
  const locationEvent =
    previous?.location?.id || previous?.locationText ? previous : input.parent;
  const locationId =
    !!input.locationId || !!input.locationText
      ? input.locationId
      : locationEvent?.location?.id;
  const locationText =
    !!input.locationId || !!input.locationText
      ? (input.locationText ?? '')
      : locationEvent?.location?.id
        ? ''
        : (locationEvent?.locationText ?? '');

  return EventFormSchema.parse({
    type,
    locationId: locationText ? 'other' : (locationId ?? 'none'),
    locationText,
    capacity: type === 'LESSON' ? 1 : 0,
    capacityUnit: type === 'LESSON' ? 'REGISTRATIONS' : 'PEOPLE',
    isVisible: true,
    instances: [
      {
        since: since.toISOString(),
        until: until.toISOString(),
      },
    ],
    trainers: trainerPersonIds.map((personId) => ({ personId, lessonsOffered: 0 })),
    registrationPeriod: fallback.lockEventsByDefault ? null : { start: null, end: null },
  });
}

export function editEventDefaults(
  event: EventWithTrainerFragment,
  data: EventFormOptionsQuery | undefined,
  now: number,
): EventFormType {
  const at = Math.min(now, Date.parse(event.since));
  const isPast = at < now;
  const cohortMembers = new Map(
    (data?.tenant?.cohortsList ?? []).map((cohort) => [
      cohort.id,
      new Set(
        cohort.cohortMembershipsList.flatMap((x) =>
          x.person &&
          (isPast || x.status === 'ACTIVE') &&
          Date.parse(x.since) <= at &&
          (!x.until || Date.parse(x.until) > at)
            ? [x.person.id]
            : [],
        ),
      ),
    ]),
  );
  return {
    name: event.name ?? '',
    type: event.type ?? 'LESSON',
    locationId: event.locationText ? 'other' : (event.location?.id ?? 'none'),
    locationText: event.locationText ?? '',
    capacity: event.capacity ?? 0,
    capacityUnit: event.capacityUnit,
    isVisible: event.isVisible ?? false,
    isPublic: event.isPublic ?? false,
    hasPublicDetails: event.hasPublicDetails,
    registrationPeriod: event.registrationPeriod,
    enableNotes: event.enableNotes ?? false,
    instances: [
      {
        itemId: event.id,
        since: event.since,
        until: event.until,
        isCancelled: event.isCancelled,
      },
    ],
    trainers: event.eventTrainersList.map((trainer) => ({
      personId: trainer.personId,
      lessonsOffered: trainer.lessonsOffered,
    })),
    cohortIds: event.targetCohortsList.map(({ cohortId }) => cohortId),
    registrations:
      data?.registrations?.map((r) => ({
        personId: r.personId,
        coupleId: r.coupleId,
        isCancelled: false,
        cohortIds:
          r.source !== 'COHORT'
            ? []
            : event.targetCohortsList
                .filter(
                  ({ cohortId }) =>
                    cohortId === r.targetCohortId ||
                    (r.personId !== null && cohortMembers.get(cohortId)?.has(r.personId)),
                )
                .map((x) => x.cohortId),
      })) ?? [],
  };
}
