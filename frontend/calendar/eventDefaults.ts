import type { SlotInfo } from '@/calendar/types';
import type { EventWithTrainerFragment } from '@/graphql/Event';
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
  fallback: { trainerPersonIds: readonly string[]; isLocked: boolean },
): EventFormType {
  const { after, events = [], type = 'LESSON' } = input;
  const since = new Date(input.since ?? after?.until ?? input.date ?? new Date());
  if (!input.since && !after) since.setHours(type === 'CAMP' ? 9 : 15, 0, 0, 0);
  const until = input.until ?? add(since, type === 'CAMP' ? 480 : 45, 'minutes');
  const trainerPersonIds = input.trainerPersonIds ??
    after?.trainersList.map((x) => x.personId) ?? fallback.trainerPersonIds;

  let previous = after;
  if (!previous) {
    for (const event of events) {
      if (
        !event.isCancelled &&
        eq(new Date(event.since), since, 'day') &&
        new Date(event.until) <= since &&
        event.trainersList.some((x) => trainerPersonIds.includes(x.personId)) &&
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
    isLocked: fallback.isLocked,
    instances: [{
      since: since.toISOString(),
      until: until.toISOString(),
    }],
    trainers: trainerPersonIds.map((personId) => ({ personId, lessonsOffered: 0 })),
  });
}
