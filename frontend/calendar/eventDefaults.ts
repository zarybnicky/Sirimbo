import type { SlotInfo } from '@/calendar/types';
import type { EventFormOptionsQuery, EventWithTrainerFragment } from '@/graphql/Event';
import { EventForm, type EventFormType } from '@/ui/event-form/types';
import { add, eq } from 'date-arithmetic';

export type CreateEventDefaults = {
  date?: Date;
  after?: EventWithTrainerFragment;
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

export function defaultsFromSlot(slot: SlotInfo): CreateEventDefaults {
  if (slot.action === 'day') return { date: slot.start };

  const { resource } = slot;
  return {
    since: slot.start,
    until: slot.action === 'select' ? slot.end : undefined,
    trainerPersonIds:
      resource?.type === 'person'
        ? [resource.id]
        : resource?.type === 'unassigned' || resource?.type === 'competition'
          ? []
          : undefined,
    locationId: resource?.type === 'location' ? resource.id : undefined,
    locationText: resource?.type === 'locationText' ? resource.id : undefined,
  };
}

export function createEventDefaults(
  input: CreateEventDefaults,
  fallbackTrainer?: string,
): EventFormType {
  const { after, events = [], type = 'LESSON' } = input;
  const since = new Date(input.since ?? after?.until ?? input.date ?? new Date());
  if (!input.since && !after) since.setHours(type === 'CAMP' ? 9 : 15, 0, 0, 0);
  const until = input.until ?? add(since, type === 'CAMP' ? 480 : 45, 'minutes');
  const trainerPersonIds = input.trainerPersonIds ??
    after?.eventTrainersList.map((x) => x.personId) ??
    (fallbackTrainer ? [fallbackTrainer] : []);

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
  const explicitLocation = input.locationId !== undefined || input.locationText !== undefined;
  const locationId = explicitLocation ? input.locationId : previous?.location?.id;
  const locationText = explicitLocation
    ? input.locationText ?? ''
    : previous?.location?.id ? '' : previous?.locationText ?? '';

  return EventForm.parse({
    type,
    locationId: locationText ? 'other' : locationId ?? 'none',
    locationText,
    capacity: type === 'LESSON' ? 1 : 0,
    capacityUnit: type === 'LESSON' ? 'REGISTRATIONS' : 'PEOPLE',
    isVisible: true,
    instances: [{
      since: since.toISOString(),
      until: until.toISOString(),
    }],
    trainers: trainerPersonIds.map((personId) => ({ personId, lessonsOffered: 0 })),
  });
}

export function editEventDefaults(
  event: EventWithTrainerFragment,
  data: EventFormOptionsQuery | undefined,
  now: number,
): EventFormType {
  const at = Math.min(now, Date.parse(event.since));
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
      cohortIds: r.source !== 'COHORT' ? [] : event.targetCohortsList
        .filter(({ cohortId }) => cohortId === r.targetCohortId ||
          data?.tenant?.cohortsList?.find((x) => x.id === cohortId)?.cohortMembershipsList
            .some((m) => m.person?.id === r.personId &&
              (at < now || m.status === 'ACTIVE') && Date.parse(m.since) <= at &&
              (!m.until || Date.parse(m.until) > at)))
        .map((x) => x.cohortId),
    })),
  };
}
