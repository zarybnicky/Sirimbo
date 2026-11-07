import type { Resource } from '@/calendar/types';
import type { EventFormOptionsQuery, EventWithTrainerFragment } from '@/graphql/Event';
import { EventFormSchema, type EventFormType } from '@/ui/event-form/types';
import { add, eq } from 'date-arithmetic';

export type CreateEventDefaults = {
  date?: Date;
  after?: EventWithTrainerFragment;
  parent?: EventWithTrainerFragment;
  events?: readonly EventWithTrainerFragment[];
  type?: EventFormType['type'];
  since?: Date;
  until?: Date;
  trainerPersonIds?: readonly string[];
  locationId?: string | null;
  locationText?: string;
};

export function splitIntoLessonRanges(since: Date, until: Date) {
  const ranges: { since: Date; until: Date }[] = [];
  let start = since;

  while (add(start, 45, 'minutes') <= until) {
    const end = add(start, 45, 'minutes');
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
  const until = input.until ?? add(since, type === 'CAMP' ? 480 : 45, 'minutes');
  const prefilledTrainerIds =
    input.trainerPersonIds ??
    after?.eventTrainersList.map((x) => x.personId) ??
    (fallback.trainerId ? [fallback.trainerId] : []);
  const trainerPersonIds = prefilledTrainerIds.filter((id) => fallback.allowedTrainerIds.includes(id));
  if (prefilledTrainerIds.length > 0 && trainerPersonIds.length === 0 && fallback.trainerId) {
    trainerPersonIds.push(fallback.trainerId);
  }

  let previous = after;
  if (!previous) {
    for (const event of events) {
      if (
        !event.isCancelled &&
        eq(new Date(event.since), since, 'day') &&
        new Date(event.until) <= since &&
        event.eventTrainersList.some((x) => trainerPersonIds.includes(x.personId)) &&
        (!previous || new Date(event.since) > new Date(previous.since))
      ) {
        previous = event;
      }
    }
  }
  const explicitLocation =
    input.locationId !== undefined || input.locationText !== undefined;
  const locationEvent = previous?.location?.id || previous?.locationText ? previous : input.parent;
  const locationId = explicitLocation ? input.locationId : locationEvent?.location?.id;
  const locationText = explicitLocation
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
        cohort.cohortMembershipsList.flatMap((membership) =>
          membership.person &&
          (isPast || membership.status === 'ACTIVE') &&
          Date.parse(membership.since) <= at &&
          (!membership.until || Date.parse(membership.until) > at)
            ? [membership.person.id]
            : [],
        ),
      ),
    ]),
  );
  const registrations = data?.registrations ?? [];
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
    cohorts: event.targetCohortsList.map(({ cohortId }) => ({ cohortId })),
    registrations: registrations.map((r) => ({
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
    })),
  };
}
